(in-package #:web-site)

(defun start-all ()
  (dao:connect)
  (api:start))

(defun stop-all ()
  (api:stop))

(defmacro build-spinneret-html-response (&body body)
  `(with-output-to-string (out)
     (let ((spinneret:*html* out))
       ,@body)))

(defroute home
  (:get "text/html")
  (build-spinneret-html-response (be-it:index)))

(defroute wcv
  (:get "text/html" cv-id)
  (build-spinneret-html-response (let ((cv (services:get-cv cv-id)))
                                   (be-it:cv->html (dto:title cv)
                                                   cv))))
