(in-package #:api)

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
    (handler-case (let* ((contact (mito:insert-dao (dao:json->dao (dao:contact-mapper) (get-in json :contact))))

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
                                                                 :cv cv)))
                         (paragraph-elements (loop for p in (get-in json :paragraphs)
                                                collect
                                                  (loop for p-elt in (get-in p :elements)
                                                     collect
                                                       (mito:create-dao 'dao:paragraph-element
                                                                   :section (get-in p :section)
                                                                   :paragraph (get-in p-elt :paragraph)
                                                                   :order (get-in p-elt :order)
                                                                   :content (json:encode-json-to-string
                                                                             (get-in p-elt :content))
                                                                   :cv cv)))))
                    (let* ((out "")
                           (out (when readings
                                  (format nil "~A~&Created ~A readings" out (length readings))))
                           (out (when work-experiences
                                  (format nil "~A~&Created ~A work-experiences" out (length work-experiences))))
                           (out (when paragraph-elements
                                  (format nil "~A~&Created ~A paragraph-elements" out (length paragraph-elements))))
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
    (clack:stop *server-handler*)))
