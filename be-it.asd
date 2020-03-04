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
               #:unix-opts)
  :components ((:module "src"
                :components
                ((:file "main")
                 (:file "dao")
                 (:file "api"))))
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
