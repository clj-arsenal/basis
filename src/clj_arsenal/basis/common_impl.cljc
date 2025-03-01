(ns ^:no-doc clj-arsenal.basis.common-impl)

(defn- gather*
  [!found form pred select]
  (let
    [selected (select form)]
    (cond
      (pred selected)
      (conj! !found selected)

      (or (coll? selected) (seq? selected))
      (reduce
        (fn [!found x]
          (gather* !found x pred select))
        !found
        selected)
      
      :else
      !found)))

(defn gather
  [form pred & {:keys [select] :or {select identity}}]
  (persistent! (gather* (transient []) form pred select)))
