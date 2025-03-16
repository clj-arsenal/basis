(ns ^:no-doc clj-arsenal.basis.common-impl
  (:require
   [clj-arsenal.basis.protocols.chain :as chain]
   [clj-arsenal.basis.protocols.err :as err]
   [clj-arsenal.basis.protocols.notifier :as notifier]
   [clj-arsenal.basis.protocols.duration :as duration]
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

(defn expand-catch-any-type
  []
  (case *expand-host*
    :cljs :default
    :cljd 'dynamic
    :clj 'Throwable))

(defn expand-use-body-try-finally
  [binding-pairs body]
  (if (empty? binding-pairs)
    body
    (let [[[first-pattern first-expr] & rest-pairs] binding-pairs]
      `(let [value# ~first-expr]
         (try
           (let [~first-pattern value#]
             ~(expand-use-body-try-finally (rest binding-pairs) body))
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
      (expand-use-body-try-finally binding-pairs body))))

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
          (recur tail (conj linear-exprs head)))))))

(defn chain
  [x continue]
  (if (satisfies? chain/Chain x)
    (chain/-chain x continue)
    (continue x))
  nil)

#?(:cljd
   (defn err-any
     ([err st]
      (when (satisfies? err/Err err)
        (impl/err (assoc (impl/err-data err) :st st :cause err)))))

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

(defrecord ^:private Placeholder [k])
(defonce ^:private !placeholder-key (volatile! 0))

(defn- placeholder
  []
  (->Placeholder (vswap! !placeholder-key #?(:cljd inc :default unchecked-inc))))

(defn- placeholder?
  [x]
  (instance? Placeholder x))

(defn chain-all
  [form continue & {:keys [mapper]}]
  (try
    (let
      [!resolved (atom {})
       mapper (or mapper identity)

       wait-resolved
       (fn wait-resolved [deps f]
         (let
           [watch-key (gensym)
            !done? (atom false)

            on-resolved
            (fn [resolved]
              (cond
                (and (satisfies? err/Err resolved) (compare-and-set! !done? false true))
                (do
                  (continue resolved)
                  (remove-watch !resolved watch-key))

                (and (every? #(contains? resolved %) deps) (compare-and-set! !done? false true))
                (do
                  (f resolved)
                  (remove-watch !resolved watch-key))))]
           (add-watch !resolved watch-key (fn [_ _ _ resolved] (on-resolved resolved)))
           (on-resolved @!resolved)))

       walked
       (walk/postwalk
         (fn [x]
           (cond
             (map-entry? x)
             x

             :else
             (let
               [deps
                (when (coll? x)
                  (filter placeholder? (if (map? x) (mapcat identity x) x)))]
               (if (empty? deps)
                 (let [x-mapped (mapper x)]
                   (if-not (satisfies? chain/Chain x-mapped)
                     x-mapped
                     (let [p (placeholder)]
                       (chain
                         x-mapped
                         (fn [x-resolved]
                           (when (map? @!resolved)
                             (if (satisfies? err/Err x-resolved)
                               (reset! !resolved x-resolved)
                               (swap! !resolved assoc p x-resolved)))))
                       p)))
                 (let [p (placeholder)]
                   (wait-resolved
                     deps
                     (fn [resolved]
                       (let [x-deps-resolved
                             (walk/walk
                               (fn [y]
                                 (cond
                                   (map-entry? y)
                                   (let [[k v] y]
                                     [(cond->> k (placeholder? k) (get resolved))
                                      (cond->> v (placeholder? v) (get resolved))])

                                   (placeholder? y)
                                   (get resolved y)

                                   :else
                                   y))
                               identity
                               x)

                             x-mapped (mapper x-deps-resolved)]
                         (if-not (satisfies? chain/Chain x-mapped)
                           (when (map? @!resolved)
                             (swap! !resolved assoc p x-mapped))
                           (chain
                             x-mapped
                             (fn [x-resolved]
                               (when (map? @!resolved)
                                 (if (satisfies? err/Err x-resolved)
                                   (reset! !resolved x-resolved)
                                   (swap! !resolved assoc p x-resolved)))))))))
                   p)))))
         form)]
      (if-not (placeholder? walked)
        (continue walked)
        (wait-resolved
          #{walked}
          (fn [resolved]
            (continue (get resolved walked))))))
    (catch #?(:cljd dynamic :clj Throwable :cljs :dynamic) ex #?@(:cljd [st])
      (err-any ex #?(:cljd st))))
  nil)
