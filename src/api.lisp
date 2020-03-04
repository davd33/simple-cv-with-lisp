(defpackage #:api
  (:use #:cl #:snooze)
  (:export #:start))

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
    (format t "hello = ~A" (cdr (assoc :title json)))
    "CV Stored."))


;; START HTTP SERVER
(defun start ()
  (clack:clackup (snooze:make-clack-app) :port 9003))
