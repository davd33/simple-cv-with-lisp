(defpackage be-it
  (:use :cl))
(in-package :be-it)

;; LOAD EVERYTHING

(load "~/quicklisp/setup.lisp")
(ql:quickload "unix-opts")
(ql:quickload "fset")
(ql:quickload "spinneret")
(require "spinneret")
(use-package :spinneret)

;; DEFINE COMMAND ARGUMENTS

(opts:define-opts
  (:name :help
   :description "Some help here needed. TODO"
   :short #\h
   :long "help"))

;; TODO
;; The parameters that we'd be interested for are the following:
;;  - the language of the page... although we could as well generate as many html files that
;;    we have from languages
;;  - the path of the output directory to which the html files should be created

;; READ PROPERTIES FILE

(defvar properties (fset:empty-map))
(let ((in (open "./resources/lang.en.properties" :if-does-not-exist nil)))
  (when in
    (loop for line = (read-line in nil)
          while line
          do (let ((split-line (split-sequence:split-sequence #\= line)))
               (setq properties (fset:with properties
                                           (first split-line)
                                           (second split-line)))))))

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

(deftag work-experience (title text)
  `(:div.card
    (:h1 ,title)
    (:p ,text)))

(defun index ()
  (with-page (:title *page-title*)
    (:section
     (:img
      :class "cv-img"
      :src "./resources/images/my.jpg"
      :alt (fset:@ properties "cv.pic.img.alt")))
    (:header                           ; CV TITLE - MY NAME BASICALLY...
     (:h1 (fset:@ properties "cv.title")))
    (:section                         ; ABOUT ME
     (:h1 (fset:@ properties "about.me"))
     (:p (fset:@ properties "about.me.txt.p1"))
     (:p (fset:@ properties "about.me.txt.p2")))
    (:section                         ; WORK EXPERIENCE
     (work-experience
       "EXP - YY"
       "Hello I'm here"))
    (:footer
     (:a
      :href "mailto:davd33@gmail.com"
      "Contact me: davd33@gmail.com"))))

;; WRITES CV TO HTML FILE
;; /mnt/linode/my/var/www/localhost/htdocs/ <- for my linode server - locally mounted
(with-open-file (cv-file "/home/davd/clisp/be-it/src/my-cv.html" :direction :output
                                                                 :if-exists :supersede)
  (let ((*html* cv-file))
    (index)))

;; exit program
;; (sb-ext:exit)
