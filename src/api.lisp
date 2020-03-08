(in-package #:api)

;; TODO
;; (defmacro get-in (json fields)
;;   (loop for f in fields
;;        ))

(defroute api-doc
  (:get :text/html)
  "<p>Helloworld</p>")

(setf snooze:*catch-errors* :verbose)

(defroute cv
  (:post "application/json")
  (let* ((json (handler-case
                   (json:decode-json-from-string (payload-as-string))
                 (error (e)
                   (http-condition 400 "Malformed JSON (~A)!" e)))))
    (handler-case (progn
                    (mito:create-dao 'dao:cv
                                     :title (cdr (assoc :title json))
                                     :sub-title (cdr (assoc :sub-title json))
                                     :image-description (cdr (assoc :image-description json)))
                    (format t "Has been stored :)")
                    "CV Stored.")
      (dbi.error:<dbi-database-error> (e)
        (format t "error during CV creation: ~A" e)
        (format nil "ERROR DB: ~A" e)))
    ))


;; START HTTP SERVER
(defparameter server-handler nil)
(defun start ()
  (setf server-handler
        (clack:clackup (snooze:make-clack-app) :port 9003)))
(defun stop ()
  (when server-handler
    (clack:stop server-handler)))
