(defsystem "be-it"
  :version "0.1.0"
  :author ""
  :license ""
  :serial t
  :depends-on (#:spinneret
               #:fset
               #:unix-opts)
  :components ((:module "src"
                :components
                ((:file "main"))))
  :description ""
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
