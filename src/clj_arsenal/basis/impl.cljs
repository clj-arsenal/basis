(ns clj-arsenal.basis.impl
  (:require
    [clj-arsenal.basis.protocols.notifier :as notifier]
    [clj-arsenal.basis.protocols.err :as err]
    [clj-arsenal.basis.protocols.duration :as duration]
    [clj-arsenal.basis.protocols.instant :as instant]
    [clj-arsenal.basis.protocols.dispose :as dispose]
    [clj-arsenal.basis.protocols.chain :as chain]))

(deftype Signal
  [!listeners finalizer]
  IFn
  (-invoke
    [_this]
    (doseq [listener (vals @!listeners)]
      (listener)))

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

(def Err
  (js* "class Err extends globalThis.Error {
    constructor(msg, data) {
      super(msg);
      this.data = data;
    }
  }"))

(extend-type Err
  err/Err
  (-data
    [^Err err]
    (.-data err)))

(defn err
  ([& {:as data}] (new Err (or (:msg data) (:p data)) data))
  ([{:as data}] (new Err (or (:msg data) (:p data)) data)))

(defn err-data
  [err]
  (cond
    (satisfies? err/Err err)
    (err/-data err)
    
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
  (js/setTimeout #(apply f args) (->ms delay)))

(defn schedule-every
  [delay f & args]
  (js/setInterval #(apply f args) (->ms delay)))

(defn cancel-scheduled
  [h]
  (js/clearTimeout h)
  nil)

(defn async
  [f & args]
  (js/Promise.
    (fn [resolve reject]
      (js/setTimeout
        (fn []
          (try
            (resolve (apply f args))
            (catch :default ex
              (reject ex))))))))

(extend-protocol err/Err
  ExceptionInfo
  (-data
    [ex]
    (merge
      {:p (.-constructor ex)
       :msg (.-message ex)}
      (ex-data ex)))
  
  js/Error
  (-data
    [ex]
    {:p (-> ex .-constructor .-name)
     :msg (.toString ex)}))

(extend-protocol instant/Instant
  js/Date
  (-milliseconds-since-epoch
    [dt]
    (.valueOf dt)))

(extend-protocol chain/Chain
  js/Promise
  (-chain
    [p continue]
    (-> p
        (.then p continue)
        (.catch p continue))
    nil))
