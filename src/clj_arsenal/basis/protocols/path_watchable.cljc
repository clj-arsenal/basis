(ns clj-arsenal.basis.protocols.path-watchable)

(defprotocol PathWatchable
  #?@(:cljd [] :default [:extend-via-metadata true])
  (-path-watch [pw k path f])
  (-path-unwatch [pw k]))
