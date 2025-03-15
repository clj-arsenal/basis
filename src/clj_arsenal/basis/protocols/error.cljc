(ns clj-arsenal.basis.protocols.error)

(defprotocol Error
  #?@(:cljd [] :default [:extend-via-metadata true])
  (-data [err]))
