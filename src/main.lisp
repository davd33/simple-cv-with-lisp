(defpackage be-it
  (:use :cl))
(in-package :be-it)

;; LOAD EVERYTHING

(load "~/quicklisp/setup.lisp")
(ql:quickload "spinneret")
(require "spinneret")
(use-package :spinneret)

;; DEFINE WEB PAGE COMPONENTS

(defparameter *page-title* "Be.it")

(defmacro concat (&body body)
  `(concatenate 'string ,@body))

(defmacro css-block (title style-str)
  `(format nil "~a {~%~a}~%" ,title ,style-str))

(defmacro css (&body styles)
  "Takes a n CSS instructions as two elements lists, then returns a css formatted string.
   A CSS command looks like this: (:font-size <string>)"
  `(concatenate 'string
                ,@(loop for style in styles
                        collect `(format nil "~a: ~a;~%" ,(string (first style)) ,(second style)))))

(defmacro with-page ((&key title) &body body)
  `(with-html
     (:doctype)
     (:html
      (:head
       (:link :href "./resources/css/cv.css" :rel "stylesheet" :type "text/css")
       (:title ,title))
      (:body
       :style (css
                (:width "80vw")
                (:max-width "1000px")
                (:margin "auto")
                (:font-size "1.5em"))
       ,@body))))

(defun index ()
  (with-page (:title *page-title*)
    (:section
     (:img
      :style (css
              (:width "200px")
              (:float "left")
              (:margin "0 50px 20px 20px")
              (:border-radius "50%")
              (:border "0")
              (:box-shadow "1px 1px 5px black"))
      :src "./resources/images/my.jpg"
      :alt "me in an image"))
    (:div
     (:header
      (:h1 "David Rueda")
      (:h2 "IT Engineer")
      (:section
       (:h1 "Programming should open people's mind.")
       (:p "I am a free software lover. I have been using free software practically my whole life, and although I liked it a lot, I began a few months ago to really understand their potential to create an open-minded and productive society."))
      (:section
       (:h1 "My Experience")
       (:section.experience
        (:h1.title "Amerbank")))
      (:footer "Contact me: davd33@gmail.com")))))

;; Let's define a macro that looks like this:
;; (csslet
;;  (exp "section.experience"
;;       (exp.title "h1.title"))
;;  (format nil "inner block = ~a" exp.title))
;; ;; And then expands to that:
;; (let ((exp "section.experience")
;;       (exp.title "section.experience h1.title"))
;;   (format nil "inner block = ~a" exp.title))
;; How should we do?
;; (defmacro csslet (&body lets)
;;   (loop for (lsymbol lvalue &rest inner-lets) in lets
;;         collect `(let ())))             ;TODO finish this...

;; (defun css4file ()
;;   (let ((section-exp-block "section.experience")
;;         (exp-1-title (concat section-exp-block "h1.title")))
;;     (concatenate 'string
;;                  (css-block exp-1-title
;;                             (css
;;                               (:font-size "2em")))
;;                  (css-block section-exp-block
;;                             (css
;;                               (:background "lightgray"))))))

;; WRITES CSS TO FILE
;; (with-open-file (css-file "./resources/css/cv.css" :direction :output
;;                                                    :if-exists :supersede)
;;   (format css-file (css4file)))

;; WRITES CV TO HTML FILE
(with-open-file (cv-file "/mnt/linode/my/var/www/localhost/htdocs/index.html" :direction :output
                                        :if-exists :supersede)
  (let ((*html* cv-file))
    (index)))
