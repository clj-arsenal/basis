(ns ^:no-doc clj-arsenal.basis.impl
  (:require
    [clj-arsenal.basis.protocols.notifier :as notifier]
    [clj-arsenal.basis.protocols.duration :as duration]
    [clj-arsenal.basis.protocols.error :as error]
    [clj-arsenal.basis.protocols.dispose :as dispose]
    [clj-arsenal.basis.common-impl :as common-impl])
  (:import
    clojure.lang.IFn
    clj-arsenal.basis.protocols.Error
    [java.util Timer TimerTask]))

(defn ^:deprecated try-fn
  [f & {catch-fn :catch finally-fn :finally}]
  (try
    (f)
    (catch Exception ex
      (when (ifn? catch-fn)
        (catch-fn ex)))
    (finally
      (when (ifn? finally-fn)
        (finally-fn)))))

(deftype Signal
  [!listeners finalizer]
  IFn
  (invoke
    [_this]
    (doseq [listener (vals @!listeners)]
      (listener)))
  (applyTo
    [this _args]
    (.invoke this))

  notifier/Notifier
  (-listen
    [_this k f]
    (swap! !listeners assoc k f))
  (-unlisten
    [_this k]
    (swap! !listeners dissoc k))

  dispose/Dispose
  (-dispose!
    [_this]
    (when (ifn? finalizer)
      (finalizer))
    nil))

(defn signal?
  [x]
  (instance? Signal x))

(defn signal
  ([] (->Signal (atom {}) nil))
  ([finalizer] (->Signal (atom {}) finalizer)))

(defonce ^:private timer (Timer. true))

(defn error
  [& {:as data}]
  (proxy [Exception Error] [(str (or (:id data) (:p data)))]
    (-data [] data)))

(defn ->ms
  [duration]
  (cond
    (satisfies? duration/Duration duration)
    (duration/-to-milliseconds duration)
    
    (number? duration)
    duration
    
    :else
    (throw
      (error
        :id ::no-conversion-to-ms
        :msg (str "Can't convert " (pr-str duration) " to milliseconds")))))

(defn schedule-once
  [delay f & args]
  (let
    [tt (proxy [TimerTask] [] (run [] (apply f args)))]
    (.schedule timer tt (long (->ms delay)))
    tt))

(defn schedule-every
  [delay f & args]
  (let [tt (proxy [TimerTask] [] (run [] (apply f args)))
        delay-ms (long (->ms delay))]
    (.schedule timer tt delay-ms delay-ms)
    tt))

(defn cancel-scheduled
  [^TimerTask tt]
  (.cancel tt)
  nil)

(defn async
  [f & args]
  (common-impl/chainable
    (fn [continue]
      (future
        (continue (apply f args))))))

(defn ^:deprecated try-fn
  [f & {catch-fn :catch finally-fn :finally}]
  (try
    (f)
    (catch Exception ex
      (when (ifn? catch-fn)
        (catch-fn ex)))
    (finally
      (when (ifn? finally-fn)
        (finally-fn)))))
