(ns test
  (:require
    [clj-arsenal.check :refer [check expect] :as check]
   [clj-arsenal.basis :refer [get-in-config]]))

(check ::nothing
  (expect = 2 1))

(defn run
  [& _]
  (check/report-all-checks-and-exit!))
