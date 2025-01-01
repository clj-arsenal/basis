(ns clj-arsenal.basis.protocols.path-watchable)

(defprotocol PathWatchable
  :extend-via-metadata true
  (-path-watch [pw k path f])
  (-path-unwatch [pw k]))

(defn path-watch
  [pw k path f]
  (-path-watch pw k path f)
  nil)

(defn path-unwatch
  [pw k]
  (-path-unwatch pw k)
  nil)
