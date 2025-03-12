(ns ^:no-doc clj-arsenal.basis.impl
  (:require
   [clj-arsenal.basis.protocols.notifier :as notifier]
   [clj-arsenal.basis.protocols.duration :as duration]
   [clj-arsenal.basis.protocols.instant :as instant]
   [clj-arsenal.basis.protocols.err :as err]
   [clj-arsenal.basis.protocols.dispose :as dispose]
   [clj-arsenal.basis.protocols.chain :as chain])
  (:import
   clojure.lang.IFn
   clojure.lang.IExceptionInfo
   clojure.lang.ExceptionInfo
   java.util.function.BiFunction
   [java.util.concurrent CompletionStage Future]
   [java.util Timer TimerTask Date]
   [java.time Duration Instant]))

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

(defn err
  [& {:as data}]
  (let [data (merge (dissoc data :data) (:data data))]
    (ExceptionInfo. (str (:msg data) (:p data)) data)))

(defn err-data
  [err]
  (cond
    (satisfies? err/Err err)
    (err/-data err)
    
    (satisfies? IExceptionInfo err)
    (ex-data err)
    
    :else
    nil))

(defn ->ms
  [duration]
  (cond
    (satisfies? duration/Duration duration)
    (duration/-milliseconds duration)
    
    (number? duration)
    duration
    
    :else
    (throw
      (err
        :p ::no-conversion-to-ms
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
  (future
    (apply f args)))

(extend-protocol err/Err
  ExceptionInfo
  (-data
    [ex]
    (merge
      {:msg (ex-message ex)}
      (ex-data ex)))
  
  Throwable
  (-data
    [ex]
    {:p (type ex)
     :msg (.toString ex)}))

(extend-protocol duration/Duration
  Duration
  (-milliseconds
    [d]
    (/ (.toNanos d) 1000000)))

(extend-protocol instant/Instant
  Instant
  (-milliseconds-since-epoch
    [i]
    (.toEpochMilli i))

  Date
  (-milliseconds-since-epoch
    [d]
    (.getTime d)))

(extend-protocol chain/Chain
  CompletionStage
  (-chain
    [cs continue]
    (.handle
      cs
      ^BiFunction
      (fn [result err]
        (continue (or err result))
        nil)))
  
  Future
  (-chain
    [fut continue]
    (try
      (continue @fut)
      (catch Throwable t
        (continue t)))))
