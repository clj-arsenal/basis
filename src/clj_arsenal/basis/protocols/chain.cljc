(ns clj-arsenal.basis.protocols.chain)

(defprotocol Chain
  #?@(:cljd [] :default [:extend-via-metadata true])
  (-chain [chainable continue]))
