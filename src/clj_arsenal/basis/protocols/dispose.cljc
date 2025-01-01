(ns clj-arsenal.basis.protocols.dispose)

(defprotocol Dispose
  :extend-via-metadata true
  (-dispose [disposable]))

(defn dispose!
  [x]
  (when (satisfies? Dispose x)
    (-dispose x))
  nil)
