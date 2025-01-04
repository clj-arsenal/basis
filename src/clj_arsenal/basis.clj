(ns clj-arsenal.basis
  (:import
   clojure.lang.IFn
   [java.util Timer TimerTask]))

(defn try-fn "
Dialect independent version of try with catch-anything.
" [f & {catch-fn :catch finally-fn :finally}]
  (try
    (f)
    (catch Exception ex
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
  (instance? Exception x))

(deftype ^:private Signal [!listeners]
  IFn
  (invoke
    [this]
    (doseq [listener (vals @!listeners)]
      (listener)))
  (applyTo
    [this args]
    (.invoke this)))

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

(defonce ^:private timer (Timer. true))

(defn schedule-once "
Dialect-independent task scheduler.  Schedules `f` to be invoked
with `args`, after `delay` millseconds.

Returns a handle that can be passed to `cancel-scheduled` to cancel
the task.
" [delay f & args]
  (let [tt (proxy [TimerTask] [] (run [] (apply f args)))]
    (.schedule timer tt ^long delay)
    tt))

(defn schedule-every "
Dialect-independent function to schedule periodic task.  Schedules
`f` to be invoked with `args` every `delay` milliseconds.

Returns a handle that can be passed to `cancel-scheduled` to
cancel it.
" [delay f & args]
  (let [tt (proxy [TimerTask] [] (run [] (apply f args)))]
    (.schedule timer tt ^long delay ^long delay)
    tt))

(defn cancel-scheduled "
Cancel a scheduled task.
" [^TimerTask tt]
  (.cancel tt))

(defn ticker "
Creates a signal and schedules it to be called every `delay`
milliseconds.  Returns a tuple `[sig scheduled]`, where `sig`
is the signal, and `scheduled` is a handle that can be passed
to `cancel-scheduled` to cancel the periodic calls.
" [^long delay]
  (let [sig (signal)
        t (schedule-every delay sig)]
    [sig #(cancel-scheduled t)]))
