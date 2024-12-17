(ns clj-arsenal.basis.protocols.chain "
  Provides a protocol, and some default implementations, for
  working with potentially asynchronous chains of execution.
" (:require
   [clojure.walk :as walk]
   [clj-arsenal.basis :refer [try-fn error?]]))

(defprotocol Chain
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

(defn chain
  [x continue]
  (if (satisfies? Chain x)
    (-chain x continue)
    (continue x))
  nil)

(defn chainable
  [f]
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

(defn chain-all
  [x continue & {:keys [walker mapper]}]
  (try-fn
    (fn []
      (let [!placeholders (atom #{})
            !resolved (atom {})
            walker (or walker walk/postwalk)
            mapper (or mapper identity)
            placeholder-ns (str (gensym))
            _ (prn :here1)
            walked (walker
                     (fn [y]
                       (let [y-mapped (mapper y)]
                         (if (satisfies? Chain y-mapped)
                           (let [placeholder (keyword placeholder-ns (gensym))]
                             (swap! !placeholders conj placeholder)
                             (chain y-mapped
                               (fn [y-resolved]
                                 (if (error? y-resolved)
                                   (reset! !resolved y-resolved)
                                   (swap! !resolved assoc placeholder y-resolved))))
                             placeholder)
                           y-mapped)))
                     x)
            _ (prn :her2)
            placeholders @!placeholders
            on-resolved (fn [_ _ _ resolved]
                          (prn :resolved resolved)
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
    :catch continue))
