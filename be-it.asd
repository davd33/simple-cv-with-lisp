(defsystem "be-it"
  :version "0.1.0"
  :author "David Rueda"
  :license "GPLv3"
  :serial t
  :depends-on (#:spinneret
               #:hunchentoot
               #:snooze
               #:dexador
               #:cl-json
               #:clack
               #:fset
               #:str
               #:mito
               #:sxql
               #:unix-opts
               #:trivia
               #:alexandria
               #:closer-mop)
  :components ((:module "src"
                :components
                ((:file "packages")
                 (:file "alists")
                 (:file "jsons")
                 (:file "hm")
                 (:file "data")
                 (:file "memoize")
                 (:file "pipe")
                 (:file "mop")
                 (:file "dao")
                 (:file "dto")
                 (:file "api")
                 (:file "be-it")
                 (:file "dev.mocks")
                 (:file "web-site")
                 (:file "services"))))
  :description "The Free custom CV compatible with all other CV/jobs plateform."
  :in-order-to ((test-op (test-op "be-it/tests"))))

(defsystem "be-it/tests"
  :author "David Rueda"
  :license "GPLv3"
  :depends-on ("be-it"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main")
                 (:file "jsons"))))
  :description "Test system for be-it"
  :perform (test-op (op c) (symbol-call :rove :run c)))
