(ns clj-arsenal.basis.protocols.chain "
  Provides a protocol, and some default implementations, for
  working with potentially asynchronous chains of execution.
" (:require
   [clojure.walk :as walk]
   [clj-arsenal.basis :refer [try-fn error?]]))

(defprotocol Chain
  #?@(:cljd [] :default [:extend-via-metadata true])
  (-chain [chainable continue] "
Calls continue (eventually) with the resolved value.
"))

#?(:cljs
   (extend-protocol Chain
     js/Promise
     (-chain
       [promise continue]
       (-> promise
         (.catch
           (fn [error]
             (if (instance? js/Error error)
               (continue error)
               (continue (js/Error. "wrapped non-error failure value" #js{:cause error})))))
         (.then
           (fn [value]
             (continue value))))
       nil))
   :cljd
   (extend-protocol Chain
     Future
     (-chain
       [future continue]
       (.then future continue .onError continue)
       nil)))

(defn chain "
If `x` satisfies `Chain`, calls `continue` with the
resolved value, once it resolves.

Returns `nil`.
" [x continue]
  (if (satisfies? Chain x)
    (-chain x continue)
    (continue x))
  nil)

(defn chainable "
Invokes `f` with a `continue` function.  Returns
something that satisfies `Chain`.  When `continue`
is called, the chainable will resolve to its argument.
" [f]
  (let [!state (atom {})]
    (f
      (fn [new-value]
        (let [[old-state new-state] (swap-vals! !state assoc :value new-value)
              existing-continue (get old-state :continue ::not-found)]
          (when (not= existing-continue ::not-found)
            (existing-continue new-value)))))

    (reify Chain
      (-chain
        [_ continue]
        (let [[old-state _] (swap-vals! !state assoc :continue continue)
              existing-value (get old-state :value ::not-found)]
          (when (not= existing-value ::not-found)
            (continue existing-value)))))))

(defrecord ^:private Placeholder [k])
(defonce ^:private !placeholder-key (volatile! 0))

(defn- placeholder
  []
  (->Placeholder (vswap! !placeholder-key #?(:cljd inc :default unchecked-inc))))

(defn- placeholder?
  [x]
  (instance? Placeholder x))

(defn chain-all "
Resolves all `Chain` implementors in `form`, calling `continue`
with the result.

If an inner chainable resolves to an `error?` value, the result
will be the first such value encountered instead of the
resolved `form`.

If `mapper` is given, each inner value of `x` will be passed
through it; allowing for substitution.  If the value is a
chainable, then the resolved value will be passed.
" [form continue & {:keys [mapper]}]
  (try-fn
    (fn []
      (let [!resolved (atom {})
            mapper (or mapper identity)

            wait-resolved
            (fn wait-resolved [deps f]
              (let [watch-key (gensym)
                    !done? (atom false)
                    
                    on-resolved
                    (fn [resolved]
                      (cond
                        (and (error? resolved) (compare-and-set! !done? false true))
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
                  (let [deps (when (coll? x) (filter placeholder? (if (map? x) (mapcat identity x) x)))]
                    (if (empty? deps)
                      (let [x-mapped (mapper x)]
                        (if-not (satisfies? Chain x-mapped)
                          x-mapped
                          (let [p (placeholder)]
                            (chain x-mapped
                              (fn [x-resolved]
                                (when (map? @!resolved)
                                  (if (error? x-resolved)
                                    (reset! !resolved x-resolved)
                                    (swap! !resolved assoc p x-resolved)))))
                            p)))
                      (let [p (placeholder)]
                        (wait-resolved deps
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
                              (if-not (satisfies? Chain x-mapped)
                                (when (map? @!resolved)
                                  (swap! !resolved assoc p x-mapped))
                                (chain x-mapped
                                  (fn [x-resolved]
                                    (when (map? @!resolved)
                                      (if (error? x-resolved)
                                        (reset! !resolved x-resolved)
                                        (swap! !resolved assoc p x-resolved)))))))))

                        p)))))
              form)]
        (if-not (placeholder? walked)
          (continue walked)
          (wait-resolved #{walked}
            (fn [resolved]
              (continue (get resolved walked)))))))
    :catch continue)
    nil)
