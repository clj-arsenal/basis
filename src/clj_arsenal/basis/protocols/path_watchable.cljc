(ns clj-arsenal.basis.protocols.path-watchable "
A protocol for something that can be watched on a
particular path within its value, which is expected
to be an associative collection.
")

(defprotocol PathWatchable
  #?@(:cljd [] :default [:extend-via-metadata true])
  (-path-watch [pw k path f])
  (-path-unwatch [pw k]))

(defn path-watch "
Watch for changes under `path` on the PathWatchable.  The
`k` is a key that can be used to remove the watch with
`path-unwatch`.  The `path` should be a vector.

The `f` will be called each time the value at `path` could
have changed.  It'll be called as `(f old-val new-val changed-paths)`,
where `old-val` is the previous value at `path`, `new-val` is the new
value, and `changed-paths` is a set of more specific subpaths that
have changed (or `#{[]}`) if the specifics aren't available.
" [pw k path f]
  (-path-watch pw k path f)
  nil)

(defn path-unwatch "
Remove a watch on the PathWatchable.  `k` is the key given to
`path-watch` when adding the watch.
" [pw k]
  (-path-unwatch pw k)
  nil)
