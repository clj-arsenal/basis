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
