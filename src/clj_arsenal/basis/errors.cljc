(ns clj-arsenal.basis.errors)

(defn error-fn
  [p msg]
  (fn [& {:as data}]
    (ex-info msg (assoc data :p p))))

