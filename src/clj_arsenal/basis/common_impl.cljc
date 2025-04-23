(ns ^:no-doc clj-arsenal.basis.common-impl
  (:require
   [clj-arsenal.basis.protocols.chain :as chain]
   [clj-arsenal.basis.protocols.err :as err]
   [clj-arsenal.basis.impl :as impl]
   [clojure.walk :as walk]))

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

(def arg-err
  (fn [msg]
    (new #?(:clj IllegalArgumentException :cljd ArgumentError :cljs js/Error) msg)))

(def ^:dynamic *expand-host* nil)

(defn- expand-catch-any-type
  []
  (case *expand-host*
    :cljs :default
    :cljd 'dynamic
    :clj 'Throwable))

(defn expand-dispose-body-try-finally
  [binding-pairs body]
  (if (empty? binding-pairs)
    body
    (let [[[first-pattern first-expr] & rest-pairs] binding-pairs]
      `(let [value# ~first-expr]
         (try
           (let [~first-pattern value#]
             ~(expand-dispose-body-try-finally rest-pairs body))
           (finally
             (clj-arsenal.basis/dispose! value#)))))))

(defn expand-with-dispose-body
  [stuff]
  (when-not (vector? (first stuff))
    (throw (arg-err "Syntax error - expected binding vector")))
  (let [[bindings & body] stuff]
    (when-not (even? (count bindings))
      (throw (arg-err "Syntax error - binding vector must have even number of forms")))
    (let [binding-pairs (partition 2 bindings)]
      (expand-dispose-body-try-finally binding-pairs body))))

(defn expand-inline-let
  [[bindings & body] body-fn]
  (when-not (vector? bindings)
    (throw (arg-err "Syntax error - expected binding vector after :let")))
  `(let ~bindings ~(body-fn body)))

(defn expand-inline-dispose
  [[bindings & body] body-fn]
  (when-not (vector? bindings)
    (throw (arg-err "Syntax error - expected binding vector after :dispose")))
  (expand-with-dispose-body (cons bindings (body-fn body))))

(defn expand-inline-catch
  [forms body-fn]
  (let [[mapper-expr binding-sym & body] forms]
    (when-not (and (symbol? binding-sym) (nil? (namespace binding-sym))) 
      (throw (arg-err "Syntax error - expected mapper-expr then binding-sym after :catch")))
    (cond
      (= :cljd *expand-host*)
      `(catch dynamic ex# st#
         (let [mapper# ~mapper-expr]
           (if-some [~binding-sym (mapper# ex# st#)]
             ~(body-fn body)
             (throw ex#))))

      :else
      `(catch ~(expand-catch-any-type) ex#
         (let [mapper# ~mapper-expr]
           (if-some [~binding-sym (mapper# ex#)]
             ~(body-fn body)
             (throw ex#)))))))

(defn expand-inline-cond
  [[conds & body] body-fn]
  (when-not (vector? conds)
    (throw (arg-err "Syntax error - expected conds vector after :cond")))
  (when-not (even? (count conds))
    (throw (arg-err "Syntax error - conds vector must have even number of forms")))
  `(cond ~@conds :else ~(body-fn body)))

(defn expand-inline-await
  [[bindings & body] body-fn]
  (when-not (vector? bindings)
    (throw (arg-err "Syntax error - expected bindings vector after :await")))
  (when-not (even? (count bindings))
    (throw (arg-err "Syntax error - bindings vector must have even number of forms")))
  (let
    [binding-pairs (partition 2 bindings)]
    `(clj-arsenal.basis/chainable
       (fn [continue#]
         (clj-arsenal.basis/chain
           (clj-arsenal.basis/chain-all-seq
             ~(mapv second binding-pairs))
           (fn [~(mapv first binding-pairs)]
             (continue# ~(body-fn body))))))))

(defn expand-m-body
  [forms]
  (loop
    [remaining forms
     linear-exprs []]
    (cond
      (empty? remaining)
      (when (seq linear-exprs)
        `(do ~@linear-exprs))

      :else
      (let [[head & tail] remaining]
        (case head
          :let `(do ~@linear-exprs ~(expand-inline-let tail expand-m-body))
          :dispose `(do ~@linear-exprs ~(expand-inline-dispose tail expand-m-body))
          :catch `(try ~@linear-exprs ~(expand-inline-catch tail expand-m-body))
          :finally `(try ~@linear-exprs (finally ~(expand-m-body tail)))
          :cond `(do ~@linear-exprs ~(expand-inline-cond tail expand-m-body))
          :await `(do ~@linear-exprs ~(expand-inline-await tail expand-m-body))
          (recur tail (conj linear-exprs head)))))))

(defn chain
  [x continue]
  (if (satisfies? chain/Chain x)
    (chain/-chain x #(chain % continue))
    (continue x))
  nil)

#?(:cljd
   (defn err-any
     [err st]
     (when (satisfies? err/Err err)
       (impl/err :data (merge {:st st :p (.-runtimeType err)} (impl/err-data err)))))

   :default
   (defn err-any
     [err]
     (when (satisfies? err/Err err)
       err)))

(defn err-where
  [f]
  (fn [err]
    (when (and (satisfies? err/Err err) (f err))
      err)))

(defn chainable
  [f]
  (try
    (let [!state (atom {})]
      (f
        (fn [new-value]
          (let
            [[old-state _new-state] (swap-vals! !state assoc :value new-value)
             existing-continue (get old-state :continue ::not-found)]
            (when (not= existing-continue ::not-found)
              (existing-continue new-value)))))

      (reify chain/Chain
        (-chain
          [_ continue]
          (let
            [[old-state _] (swap-vals! !state assoc :continue continue)
             existing-value (get old-state :value ::not-found)]
            (when (not= existing-value ::not-found)
              (continue existing-value))))))
    (catch #?(:cljd dynamic :clj Throwable :cljs :dynamic) ex #?@(:cljd [st])
      (err-any ex #?(:cljd st)))))

(defn chain-all-seq
  [s]
  (loop
    [remaining s
     !done (transient [])]
    (if (empty? remaining)
      (seq (persistent! !done))
      (let
        [[next-item & rest-items] remaining]
        (if-not (satisfies? chain/Chain next-item)
          (recur rest-items (conj! !done next-item))
          (chainable
            (fn [continue]
              (chain next-item
                (fn [next-item-resolved]
                  (chain (chain-all-seq rest-items)
                    (fn [rest-items-resolved]
                      (continue
                        (concat
                          (persistent! !done)
                          [next-item-resolved]
                          rest-items-resolved)))))))))))))

(defn chain-all-coll
  [coll]
  (cond
    (map? coll)
    (chainable
      (fn [continue]
        (->
          (chain-all-seq
            (map
              (fn [[k v :as e]]
                (cond
                  (and (satisfies? chain/Chain k) (satisfies? chain/Chain v))
                  (chainable
                    (fn [continue]
                      (chain k
                        (fn [k-resolved]
                          (chain v
                            (fn [v-resolved]
                              (continue [k-resolved v-resolved])))))))

                  (satisfies? chain/Chain k)
                  (chainable
                    (fn [continue]
                      (chain k
                        (fn [k-resolved]
                          (continue [k-resolved v])))))

                  (satisfies? chain/Chain v)
                  (chainable
                    (fn [continue]
                      (chain v
                        (fn [v-resolved]
                          (continue [k v-resolved])))))

                  :else
                  e))
              coll))
          (chain
            (fn [entries-resolved]
              (continue (with-meta (into {} entries-resolved) (meta coll))))))))

    (seq? coll)
    (chainable
      (fn [continue]
        (chain (chain-all-seq coll)
          (fn [values]
            (continue (with-meta values (meta coll)))))))

    :else
    (chainable
      (fn [continue]
        (chain (chain-all-seq coll)
          (fn [resolved]
            (continue (with-meta (into (empty coll) resolved) (meta coll)))))))))
