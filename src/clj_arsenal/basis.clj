(ns clj-arsenal.basis
  (:import clojure.lang.IFn))

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
    []
    (doseq [listener (vals @!listeners)]
      (listener))))

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
