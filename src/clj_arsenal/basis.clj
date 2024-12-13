(ns clj-arsenal.basis
  (:import
   clojure.lang.IFn
   [java.util Timer TimerTask]))

(defn try-fn
  [f & {catch-fn :catch finally-fn :finally}]
  (try
    (f)
    (catch Exception ex
      (when (ifn? catch-fn)
        (catch-fn ex)))
    (finally
      (when (ifn? finally-fn)
        (finally-fn)))))

(defn error?
  [x]
  (instance? Exception x))

(deftype ^:private Signal [!listeners]
  IFn
  (invoke
    [this]
    (doseq [listener (vals @!listeners)]
      (listener)))
  (applyTo
    [this args]
    (.invoke this)))

(defn signal
  []
  (->Signal (atom {})))

(defn sig-listen
  ([^Signal sig f]
   (sig-listen sig f f))
  ([^Signal sig k f]
   (swap! (.-!listeners sig) assoc k f)))

(defn sig-unlisten
  [^Signal sig k]
  (swap! (.-!listeners sig) dissoc k))

(defonce ^:private timer (Timer. true))

(defn schedule-once
  [delay f & args]
  (let [tt (proxy [TimerTask] [] (run [] (apply f args)))]
    (.schedule timer tt ^long delay)
    tt))

(defn schedule-every
  [delay f & args]
  (let [tt (proxy [TimerTask] [] (run [] (apply f args)))]
    (.schedule timer tt ^long delay ^long delay)
    tt))

(defn cancel-scheduled
  [^TimerTask tt]
  (.cancel tt))

(defn ticker
  [^long delay]
  (let [sig (signal)
        t (schedule-every delay sig)]
    [sig #(cancel-scheduled t)]))
