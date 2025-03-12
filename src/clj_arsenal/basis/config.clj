(ns clj-arsenal.basis.config)

(defonce deps-basis
  (try
    (some->
      (requiring-resolve 'clojure.java.basis/current-basis)
      (apply nil))
    (catch Exception _
      nil)))

(defn get-in-config
  ([path]
   (get-in-config path nil))
  ([path default]
   (get-in (:argmap deps-basis) path
     (get-in deps-basis path default))))
