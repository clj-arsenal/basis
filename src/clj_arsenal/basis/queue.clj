(ns clj-arsenal.basis.queue "
Dialect-independent implementation of a FIFO queue.
" (:import
   clojure.lang.PersistentQueue))

(def empty-queue "
An empty queue.
" PersistentQueue/EMPTY)
