(ns clj-arsenal.basis.protocols.dispose)

(defprotocol Dispose
  (-dispose [disposable]))

(defn dispose!
  [x]
  (when (satisfies? Dispose x)
    (-dispose x))
  nil)
