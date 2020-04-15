(in-package #:api)

(defroute api-doc
  (:get :text/html)
  "<p>Helloworld</p>")

(setf snooze:*catch-errors* :verbose)

(defun cv-handler (payload-as-string)
  (let* ((json (handler-case
                   (json:decode-json-from-string payload-as-string)
                 (error (e)
                   (http-condition 400 "Malformed JSON (~A)!" e)))))

    (services:store-cv (get-in json :contact)
                       (get-in json :reading-list)
                       (get-in json :work-experience-list)
                       (get-in json :section-list)
                       json)))

(defroute cv
  (:post "application/json")
  (cv-handler (payload-as-string)))


;;; UTILITY

(defun app-root ()
  (fad:pathname-as-directory
   (make-pathname :name nil
                  :type nil
                  :defaults #.(or *compile-file-truename* *load-truename*))))

;; START HTTP SERVER
(defclass snooze-acceptor (hunchentoot:easy-acceptor) ())

(defparameter *lispdoc-dispatch-table*
  (list
   (hunchentoot:create-folder-dispatcher-and-handler
    "/images/" (fad:pathname-as-directory #P"./resources/images"))
   (hunchentoot:create-folder-dispatcher-and-handler
    "/css/" (fad:pathname-as-directory #P"./resources/css"))
   (hunchentoot:create-folder-dispatcher-and-handler
    "/docs/" (fad:pathname-as-directory #P"./resources/docs"))
   (hunchentoot:create-folder-dispatcher-and-handler
    "/webfonts/" (fad:pathname-as-directory #P"./resources/webfonts"))
   (make-hunchentoot-app '((*home-resource* . web-site:home)))))

(defmethod hunchentoot:acceptor-dispatch-request :around ((a snooze-acceptor) request)
  (let ((hunchentoot:*dispatch-table* *lispdoc-dispatch-table*))
    (call-next-method)))

(defvar *server* nil)

(defun stop ()
  (when *server* (hunchentoot:stop *server*) (setq *server* nil)))

(defun start (&key (port 5000))
  (stop)
  (setq *server*
        (hunchentoot:start (make-instance 'snooze-acceptor :port port))))
