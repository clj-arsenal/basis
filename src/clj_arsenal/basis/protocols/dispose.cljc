(ns clj-arsenal.basis.protocols.dispose "
Dialent-independent interface for a resource that
needs to be cleaned up after use.
")

(defprotocol Dispose
  #?@(:cljd [] :default [:extend-via-metadata true])
  (-dispose [disposable]))

(defn dispose! "
If `x` satisfies `Dispose`, disposes it.  Otherwise
does nothing.
" [x]
  (when (satisfies? Dispose x)
    (-dispose x))
  nil)
