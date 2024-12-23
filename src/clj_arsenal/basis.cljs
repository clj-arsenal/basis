(ns clj-arsenal.basis)

(defn try-fn
  [f & {catch-fn :catch finally-fn :finally}]
  (try
    (f)
    (catch :default ex
      (when (ifn? catch-fn)
        (catch-fn ex)))
    (finally
      (when (ifn? finally-fn)
        (finally-fn)))))

(defn error?
  [x]
  (instance? js/Error x))

(deftype ^:private Signal [!listeners]
  IFn
  (-invoke
    [this]
    (doseq [listener (vals @!listeners)]
      (listener))))

(defn signal
  []
  (->Signal (atom {})))

(defn signal?
  [x]
  (instance? Signal x))

(defn sig-listen
  ([^Signal sig f]
   (sig-listen sig f f))
  ([^Signal sig k f]
   (swap! (.-!listeners sig) assoc k f)))

(defn sig-unlisten
  [^Signal sig k]
  (swap! (.-!listeners sig) dissoc k))

(defn schedule-once
  [delay f & args]
  (js/setTimeout #(apply f args) delay))

(defn schedule-every
  [delay f & args]
  (js/setInterval #(apply f args) delay))

(defn cancel-scheduled
  [handle]
  (js/clearTimeout handle))

(defn ticker
  [delay]
  (let [sig (signal)
        t (schedule-every delay sig)]
    [sig #(cancel-scheduled t)]))
