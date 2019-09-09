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

;; SETUP LOCALIZATION

(defun read-property-file (property-file-path)
  (let* ((in (open property-file-path :if-does-not-exist nil))
         (property-file-lines (when in
                                (loop for line = (read-line in nil)
                                      while line collect line))))
    (reduce #'(lambda (properties-map line)
                (let* ((split-line (split-sequence:split-sequence #\= line))
                       (key_index (split-sequence:split-sequence #\_ (first split-line))) ; in the case of multiple key/val indexed translations
                       (key (first key_index))
                       (index (parse-integer (or (second key_index) "NaN") :junk-allowed t))
                       (value (second split-line)))
                  (format t "key / index / value = ~a / ~a / ~a ~%" key index value)
                  (if index ; the translation should be part of a split sequence of strings
                      (fset:with properties-map
                                 key
                                 (if (fset:@ properties-map key) ; it's not the first string of this translation key
                                     (fset:with (fset:@ properties-map
                                                        key)
                                                index
                                                value)
                                     (fset:seq value)))
                      (fset:with properties-map
                                 key
                                 value))))
            property-file-lines
            :initial-value (fset:empty-map))))

(defparameter lang (read-property-file "./resources/lang.en.properties"))

(defun lang-get (key)
  "Get the translation for the given key."
  (fset:@ lang key))

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

(deftag work-experience (text attrs &key title)
  `(:div.card
    (:h1 ,title)
    (:p ,text)))

(deftag repeat (template attrs &key for-lang)
  "This is a tag that repeats a given template using the key
   for a translation split into a list of several strings.
     - lang-binding-form: 2 elements list with var name and translation key
     - template: a single form (one list of potentially embedded tags)"
  (let ((lang-var-name (first for-lang))
        (lang-key (second for-lang)))
    (fset:reduce #'(lambda (tag-product translation-value)
                     (append
                      tag-product
                      `((let ((,lang-var-name ,translation-value))
                          ,@template))))
                 (lang-get lang-key)
                 :initial-value `(:div))))

(defun index ()
  (with-page (:title *page-title*)
    (:section
     (:img
      :class "cv-img"
      :src "./resources/images/my.jpg"
      :alt (lang-get "cv.pic.img.alt")))
    (:header                            ; CV TITLE - MY NAME BASICALLY...
     (:h1 (lang-get "cv.title")))
    (:section                           ; ABOUT ME
     (:h1 (lang-get "about.me"))
     (repeat
       :for-lang (about-me "about.me.txt.p")
       (:p.about-me about-me)))
    (:section                           ; WORK EXPERIENCE
     (work-experience
       :title "Experience 1"
       "Hello I'm here"))
    (:footer
     (:p
      "Contact me: "
      (:a
       :href "mailto:davd33@gmail.com"
       "davd33@gmail.com")))))

;; WRITES CV TO HTML FILE
;; /mnt/linode/my/var/www/localhost/htdocs/ <- for my linode server - locally mounted
(with-open-file (cv-file "/home/davd/clisp/be-it/src/my-cv.html" :direction :output
                                                                 :if-exists :supersede)
  (let ((*html* cv-file))
    (index)))

;; exit program
;; (sb-ext:exit)
