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
    (handler-case (let* ((contact (dao:insert-contact (get-in json :contact)))

                         (cv (dao:insert-cv (acons :contact contact json)))

                         (readings (loop for r in (get-in json :readings)
                                      collect
                                        (dao:insert-reading (acons :cv cv r))))

                         (work-experiences (loop for we in (get-in json :work-experiences)
                                              collect
                                                (dao:insert-work-experience
                                                 (alists:aconses we
                                                                 `(:technologies
                                                                   ,(format nil
                                                                            "~{~A~^,~}"
                                                                            (get-in we :technologies)))
                                                                 `(:cv ,cv)))))
                         (paragraph-elements (loop for p in (get-in json :paragraphs)
                                                collect
                                                  (loop for p-elt in (get-in p :elements)
                                                     collect
                                                       (dao:insert-paragraph-element
                                                        (alists:aconses p-elt
                                                                        `(:section ,(get-in p :section))
                                                                        `(:content ,(json:encode-json-to-string
                                                                                     (get-in p-elt :content)))
                                                                        `(:cv ,cv)))))))
                    (let* ((response (acons :command "Create CV." nil))
                           (response (when readings
                                       (jsons:add-value (length readings) response
                                                        :created :readings)))
                           (response (when work-experiences
                                       (jsons:add-value (length work-experiences) response
                                                        :created :work-experiences)))
                           (response (when paragraph-elements
                                       (jsons:add-value (length paragraph-elements) response
                                                        :created :paragraphs-elements)))
                           (response (when cv
                                       (jsons:add-value "CV Stored" response
                                                        :created :cv))))
                      (json:encode-json-alist-to-string response)))
      (dbi.error:<dbi-database-error> (e)
        (format t "~&error during CV creation: ~A" e)
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
