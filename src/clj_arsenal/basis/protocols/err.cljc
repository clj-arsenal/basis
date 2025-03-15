(ns clj-arsenal.basis.protocols.err)

(defprotocol Err
  #?@(:cljd [] :default [:extend-via-metadata true])
  (-data [err]))
