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

(defn chain-all "
Walks the form `x`, waiting for anything the satisfies `Chain`
to resolve.

Returns a chainable, which resolves to `x` with all its inner
chainables resolved.

If an inner chainable resolves to an `error?` value, the resolved
value will be the first such value encountered instead of the
resolved `x`.

If `walker` is given it'll be used in place of `postwalk` to
walk `x`.

If `mapper` is given, each inner value of `x` will be passed
through it; allowing for substitution.  If the value is a
chainable, then the resolved value will be passed.
" [x & {:keys [walker mapper]}]
  (chainable
    (fn [continue]
      (try-fn
        (fn []
          (let [!placeholders (atom #{})
                !resolved (atom {})
                walker (or walker walk/postwalk)
                mapper (or mapper identity)
                placeholder-ns (str (gensym))
                walked (walker
                         (fn [y]
                           (let [y-mapped (mapper y)]
                             (if (satisfies? Chain y-mapped)
                               (let [placeholder (keyword placeholder-ns (str (gensym)))]
                                 (swap! !placeholders conj placeholder)
                                 (chain y-mapped
                                   (fn [y-resolved]
                                     (if (error? y-resolved)
                                       (reset! !resolved y-resolved)
                                       (swap! !resolved assoc placeholder y-resolved))))
                                 placeholder)
                               y-mapped)))
                         x)
                placeholders @!placeholders
                on-resolved (fn [_ _ _ resolved]
                              (cond
                                (error? resolved)
                                (do
                                  (continue resolved)
                                  (remove-watch !resolved ::resolved))

                                (= (count resolved) (count placeholders))
                                (do
                                  (continue
                                    (walk/postwalk
                                      (fn [y]
                                        (if (and (keyword? y) (= (namespace y) placeholder-ns))
                                          (get resolved y y)
                                          y))
                                      walked))
                                  (remove-watch !resolved ::resolved))))]
            (if (empty? placeholders)
              (continue walked)
              (do
                (add-watch !resolved ::resolved on-resolved)
                (on-resolved nil nil nil @!resolved)))
            nil))
        :catch continue))))
