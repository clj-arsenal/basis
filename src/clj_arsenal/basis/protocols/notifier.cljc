(ns clj-arsenal.basis.protocols.notifier)

(defprotocol Notifier
  (-listen [n k f])
  (-unlisten [n k]))
