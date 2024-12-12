(ns clj-arsenal.basis.protocols.chain "
  Provides a protocol, and some default implementations, for
  working with potentially asynchronous chains of execution.
")


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
       nil)

     default
     (-chain
       [value continue]
       (continue value)
       nil))
  :clj
  (extend-protocol Chain
    Object
    (-chain
      [value continue]
      (continue value))
    
    nil
    (-chain
      [value continue]
      (continue value))))

(defn chain
  [x continue]
  (-chain x continue)
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
