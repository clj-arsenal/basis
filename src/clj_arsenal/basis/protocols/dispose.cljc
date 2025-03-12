(ns clj-arsenal.basis.protocols.dispose)

(defprotocol Dispose
  #?@(:cljd [] :default [:extend-via-metadata true])
  (-dispose! [disposable]))
