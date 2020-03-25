(defsystem "be-it"
  :version "0.1.0"
  :author "David Rueda"
  :license "GPLv3"
  :serial t
  :depends-on (#:spinneret
               #:snooze
               #:cl-json
               #:clack
               #:fset
               #:mito
               #:sxql
               #:unix-opts
               #:trivia
               #:alexandria)
  :components ((:module "src"
                :components
                ((:file "packages")
                 (:file "jsons")
                 (:file "hm")
                 (:file "alists")
                 (:file "html")
                 (:file "dao")
                 (:file "api")
                 (:file "dev.mocks"))))
  :description "The Free custom CV compatible with all other CV/jobs plateform."
  :in-order-to ((test-op (test-op "be-it/tests"))))

(defsystem "be-it/tests"
  :author ""
  :license ""
  :depends-on ("be-it"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for be-it"
  :perform (test-op (op c) (symbol-call :rove :run c)))
