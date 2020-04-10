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
  (:get "text/html" cv-name)
  (build-spinneret-html-response (be-it:cv->html (str:downcase cv-name)
                                                 (services:get-cv (str:downcase cv-name)))))
