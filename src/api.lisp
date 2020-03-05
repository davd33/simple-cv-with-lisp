(defpackage #:api
  (:use #:cl #:snooze)
  (:export #:start)
  (:shadowing-import-from #:dao))

(in-package #:api)

(defroute api-doc
  (:get :text/html)
  "<p>Helloworld</p>")

(defroute cv
  (:post "application/json")
  (let* ((json (handler-case
                   (json:decode-json-from-string (payload-as-string))
                 (error (e)
                   (http-condition 400 "Malformed JSON (~A)!" e)))))
    (handler-case (progn
                    (mito:create-dao 'dao:cv
                                     :title (cdr (assoc :title json))
                                     :sub-title "coucou qui est la"
                                     :image-description "je suis ici")
                    "CV Stored.")
      (dbi.error:<dbi-database-error> (e)
        (format t "error during CV creation: ~A" e)
        "ERROR DB"))
    ))


;; START HTTP SERVER
(defun start ()
  (clack:clackup (snooze:make-clack-app) :port 9003))
