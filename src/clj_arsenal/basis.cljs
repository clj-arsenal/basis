(ns clj-arsenal.basis
  (:require
   [clj-arsenal.basis.common-impl :as impl]))

(defn try-fn "
Dialect independent version of try with catch-anything.
" [f & {catch-fn :catch finally-fn :finally}]
  (try
    (f)
    (catch :default ex
      (when (ifn? catch-fn)
        (catch-fn ex)))
    (finally
      (when (ifn? finally-fn)
        (finally-fn)))))

(defn error? "
Dialect-independent error check predicate.  Checks for
exceptions, errors, throwables, ex-info, etc.  Whatever
makes sense to be considered as, and handled as, an
error for the host language.
" [x]
  (instance? js/Error x))

(deftype ^:private Signal [!listeners]
  IFn
  (-invoke
    [this]
    (doseq [listener (vals @!listeners)]
      (listener))))

(defn signal "
Creates a signal, which is a callable that, when called,
invokes all listener callbacks attached to it.  Listeners
can be registered with `sig-listen` and `sig-unlisten`.
" []
  (->Signal (atom {})))

(defn signal? "
Returns true if `x` is a signal.
" [x]
  (instance? Signal x))

(defn sig-listen "
Attach a listener to `sig`, a signal. `k` is a key
that can be used to remove the listener via `sig-unlisten`;
if omitted then `f` doubles as the key.

`f` is the callback to be invoked when the signal is called.
" ([^Signal sig f]
   (sig-listen sig f f))
  ([^Signal sig k f]
   (swap! (.-!listeners sig) assoc k f)))

(defn sig-unlisten "
Removes a signal listener with the key `k`.
" [^Signal sig k]
  (swap! (.-!listeners sig) dissoc k))

(defn schedule-once "
Dialect-independent task scheduler.  Schedules `f` to be invoked
with `args`, after `delay` millseconds.

Returns a handle that can be passed to `cancel-scheduled` to cancel
the task.
" [delay f & args]
  (js/setTimeout #(apply f args) delay))

(defn schedule-every "
Dialect-independent function to schedule periodic task.  Schedules
`f` to be invoked with `args` every `delay` milliseconds.

Returns a handle that can be passed to `cancel-scheduled` to
cancel it.
" [delay f & args]
  (js/setInterval #(apply f args) delay))

(defn cancel-scheduled "
Cancel a scheduled task.
" [handle]
  (js/clearTimeout handle))

(defn ticker "
Creates a signal and schedules it to be called every `delay`
milliseconds.  Returns a tuple `[sig scheduled]`, where `sig`
is the signal, and `scheduled` is a handle that can be passed
to `cancel-scheduled` to cancel the periodic calls.
" [delay]
  (let [sig (signal)
        t (schedule-every delay sig)]
    [sig #(cancel-scheduled t)]))

(defn gather "
Recursively looks through a form for items for which
`pred` is truthy.  Returns a vector of all such items.

If `select` is given, then for each form/subform it'll
be called to choose the parts of the form that should
be scanned.  It receives the form, and should return
the thing that should be scanned.
" [form pred & {:keys [select] :or {select identity}}]
  (impl/gather form pred :select select))
