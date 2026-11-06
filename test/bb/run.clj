(require 'clj-arsenal.basis.config)
(require 'clj-arsenal.basis.test)
(require '[clj-arsenal.check :as check])

(assert
  (some? clj-arsenal.basis.config/deps-basis)
  "bb config discovery failed; checks would silently not register")

(check/report-all-checks-and-exit!)
