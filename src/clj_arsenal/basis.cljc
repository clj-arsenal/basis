(ns clj-arsenal.basis
  (:require
   [clj-arsenal.basis.common-impl :as common-impl]
   [clj-arsenal.basis.impl :as impl]
   [clj-arsenal.basis.protocols.err :as err]
   [clj-arsenal.basis.protocols.notifier :as notifier]
   [clj-arsenal.basis.protocols.dispose :as dispose]
   [clj-arsenal.basis.protocols.path-watchable :as path-watchable]
   #?@(:cljd []
       :clj [[clj-arsenal.basis.config :as config]]))
  #?(:cljd
     (:host-ns
      (:require
       [clj-arsenal.basis.common-impl :as common-impl]
       [clj-arsenal.basis.config :as config]))

     :cljs
     (:require-macros clj-arsenal.basis)))

#?(:clj
   (defmacro get-in-config "
Grab something from the deps basis.  Careful, this inlines
the result during macro expansion, and doesn't quote.
"
     ([path default]
      (config/get-in-config path default))
     ([path]
      (config/get-in-config path))))

#?(:cljd
   (defmacro with-dispose "
Execute the body, calling `dispose!` on all bound values
when leaving the body.

```
(with-dispose
  [res1 (create-resource)
   res2 (create-resource)]
  ...)
```
" [& stuff]
     (binding [common-impl/*expand-host* :cljd]
       (common-impl/expand-with-dispose-body stuff)))

   :clj
   (defmacro with-dispose "
Execute the body, calling `dispose!` on all bound values
when leaving the body.

```
(with-dispose
  [res1 (create-resource)
   res2 (create-resource)]
  ...)
```
" [& stuff]
     (binding [common-impl/*expand-host* (if (:ns &env) :cljs :clj)]
       (common-impl/expand-with-dispose-body stuff))))

#?(:cljd
   (defmacro m "
Multi-faceted block.  Basically a `do` with special inline directives
to mitigate nesting and allow for more concise code.

```
(m
  ... some stufff...
  :catch b/any-err err

  ... some more stuff ...

  :let
  [...bindings]

  :cond
  [my-pred-1 return-value-1
   my-pred-2 return-value-2]

  :await
  [p1 parallel-expression-1
   p2 parallel-expression-2]

  :dispose
  [r1 my-resource-1
   r2 my-resource-2]

  :finally
  ...finally body...)
```
" [& stuff]
     (binding [common-impl/*expand-host* :cljd]
       (common-impl/expand-m-body stuff)))

   :clj
   (defmacro m "
Multi-faceted block.  Basically a `do` with special inline directives
to mitigate nesting and allow for more concise code.

```
(m
  ... some stufff...
  :catch b/any-err err

  ... some more stuff ...

  :let
  [...bindings]

  :cond
  [my-pred-1 return-value-1
   my-pred-2 return-value-2]

  :await
  [p1 parallel-expression-1
   p2 parallel-expression-2]

  :dispose
  [r1 my-resource-1
   r2 my-resource-2]

  :finally
  ...finally body...)
```
" [& stuff]
     (binding [common-impl/*expand-host* (if (:ns &env) :cljs :clj)]
       (common-impl/expand-m-body stuff))))


(def
  ^{:arglists
    '[[x]]
    
    :doc "Return true if `x` is a signal"}
  signal? impl/signal?)

(def
  ^{:arglists
    '[[]
      [finalizer]]
    
    :doc "
Create a signal.  Listen with `notifier-listen`, stop listening
with `notifier-unlisten`, and invoke all listeners by calling the
the signal.

If a `finalizer` is provided, it will be called when the signal
is disposed.
"} signal impl/signal)

(def
  ^{:arglists
    '[[& data]]
    
    :doc "
Create an error value with the given data.
"} err impl/err)


(def
  ^{:arglists
    '[[err]]
    
    :doc "
Returns the data associated with `err` if it's an error value.
Otherwise `null`.
"} err-data impl/err-data)

(defn err?
  [x]
  (satisfies? err/Err x))

(defn notifier-listen "
Add a listener to something that satisfies the Notifier protocol.
" ([notifier f] (notifier/-listen notifier f f))
  ([notifier k f] (notifier/-listen notifier k f)))

(defn notifier-unlisten "
Remove a listener from something that satisfies the Notifier protocol.
" [notifier k]
  (notifier/-unlisten notifier k))

(def
  ^{:arglists
    '[[delay f & args]]
    
    :doc "
Schedule `f` to be called with `args` after waiting the given `delay`.

Delay can be a number (in milliseconds) or anything that satisfies
the Duration protocol.

Returns something that can be cancelled via `cancel-scheduled`.
"} schedule-once impl/schedule-once)

(def
  ^{:arglists
    '[[delay f & args]]
    
    :doc "
Schedule `f` to be called with `args` after waiting the given `delay`,
and then subsequently after every `delay` until cancelled.

Delay can be a number (in milliseconds) or anything that satisfies
the Duration protocol

Returns something that can be cancelled via `cancel-scheduled`.
"} schedule-every impl/schedule-every)

(def
  ^{:arglists
    '[[x]]

    :doc "Cancel something that was scheduled via `schedule-once` or `schedule-every`"} 
    cancel-scheduled impl/cancel-scheduled)

(def
  ^{:arglists
    '[[f & args]]
    
    :doc "Call `f` with `args` asynchronously.  Returns something that satisfies Chain."}
    async impl/async)

(defn clock "
Creates a signal that triggers every `period`.  Must dispose.
" [period]
  (let
    [!t (volatile! nil)
     sig (signal #(cancel-scheduled @!t))
     t (schedule-every period sig)]
    (vreset! !t t)
    sig))

(def gather
  ^{:arglists
    '[[form pred & {:keys [select] :or {select identity}}]]
    
    :doc "
Looks through any `form` for things for which `pred` is truthy.  Returns
a vector of found items.

If `select` is provided, it will be called on each subform before
traversal; allowing parts to be skipped or filtered.
"} common-impl/gather)

(def
  ^{:arglists
    '[[x]]
    
    :doc "
Tries to convert `x` to a number of milliseconds.  `x` should be a
number or something that satisfies Duration.
"} ->ms impl/->ms)

(defn dispose! "
If `x` satisfies Dispose, invokes its dispose implementation.  Otherwise
does nothing.
" [x]
  (when (satisfies? dispose/Dispose x)
    (dispose/-dispose! x))
  nil)

(def chain
  ^{:arglists
    '[[v f]]
    
    :doc "
If `v` satisfies the Chain protocol, sets `f` up to be called with the
async value when it's ready.  Otherwise immediately calls `f` with `v`.

Returns `nil`, so (despite the name) multiple invocations of `chain`
cannot be chained.  For example the following won't work:

```
(-> v
  (chain f1)
  (chain f2))
```

Instead you'd need:

```
(chain
  (chainable
    (fn [continue]
      (chain v #(continue (f1 %)))))
  f2)
```

Or use an m-block:

```
(m
  :await [v1 v]
  :await [v2 (f1 v1)]
  (f2 v2))
```
"} common-impl/chain)

(def 
  ^{:arglists
    '[[form continue & {:keys [mapper]}]]
    
    :doc "
Walks the given `form`, resolving anything that satisfies Chain, then
calling `continue` with the resulting form.

If any of the values that satisfy Chain resolve to an error, immediately
calls `continue` with the error.

If a `mapper` is given, it will be called to substitute each subform before
checking if it satisfies Chain.

Returns `nil`.
"} chain-all common-impl/chain-all)

(def
  ^{:arglists
    '[[f]]
    
    :doc "
Calls `f` with a `continue` function.  Returns something that satisfies
Chain, and resolves to the value passed to `continue`, when it's called.
"} chainable common-impl/chainable)

(defn path-watch "
Watch a path on something that satisfies PathWatchable.

The `f` will be called with `(old-val, new-val, changed-paths)` each
time something under the `path` changes.  The `changed-paths` will be
a set indicating more specifically the subpaths that were changed, or
`#{[]}` if not known.
" [pw k path f]
  (path-watchable/-path-watch pw k path f)
  nil)

(defn path-unwatch "
Remove a path watch.  The `k` should be the key originally passed to
`path-watch`.
" [pw k]
  (path-watchable/-path-unwatch pw k)
  nil)

(def err-any "
Use with `(m :catch err-any err ...)`.  Matches any error.
" common-impl/err-any)

(def err-when "
Use with `(m :catch (err-where pred) err ...)`.  Matches any
error for which `pred` is truthy.
" common-impl/err-where)
