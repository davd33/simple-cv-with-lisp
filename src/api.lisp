(in-package #:api)

(defun get-in (json &rest fields)
  (first (last (loop for f in fields
                  for res = (cdr (assoc f json)) then (cdr (assoc f res))
                  collect res))))

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
    (handler-case (let* ((contact (mito:create-dao 'dao:contact
                                                   :mail (get-in json :contact :mail)
                                                   :linkedin (get-in json :contact :linkedin)
                                                   :github (get-in json :contact :github)))

                         (cv (let ((title (get-in json :title))
                                   (sub-title (get-in json :sub-title))
                                   (image-description (get-in json :image-description)))

                               (when (= 0 (mito:count-dao 'dao:cv :title title))
                                 (mito:create-dao 'dao:cv
                                                  :title title
                                                  :sub-title sub-title
                                                  :image-description image-description
                                                  :contact contact))))

                         (readings (loop for r in (get-in json :readings)
                                      collect
                                        (mito:create-dao 'dao:reading
                                                         :title (get-in r :title)
                                                         :image (get-in r :image)
                                                         :external-url (get-in r :external-url)
                                                         :cv cv)))

                         (work-experiences (loop for we in (get-in json :work-experiences)
                                              collect
                                                (mito:create-dao 'dao:work-experience
                                                                 :title (get-in we :title)
                                                                 :company (get-in we :company)
                                                                 :description (get-in we :description)
                                                                 :technologies (format nil
                                                                                       "~{~A~^,~}"
                                                                                       (get-in we :technologies))
                                                                 :cv cv))))
                    (let* ((out "")
                           (out (when readings
                                  (format nil "~A~&Created ~A readings" out (length readings))))
                           (out (when work-experiences
                                  (format nil "~A~&Created ~A work-experiences" out (length work-experiences))))
                           (out (when cv
                                  (format nil "~A~&CV Stored: ~A" out (slot-value cv 'dao:title)))))
                      (or out "Nothing updated.")))
      (dbi.error:<dbi-database-error> (e)
        (format t "error during CV creation: ~A" e)
        (format nil "ERROR DB: ~A" e)))
    ))


;; START HTTP SERVER
(defparameter *server-handler* nil)
(defun start ()
  (setf *server-handler*
        (clack:clackup (snooze:make-clack-app) :port 9003)))
(defun stop ()
  (when *server-handler*
    (clack:stop server-handler)))
