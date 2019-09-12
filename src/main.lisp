(defpackage be-it
  (:use :cl))
(in-package :be-it)

;; LOAD EVERYTHING TODO Do better than this...

(load "~/quicklisp/setup.lisp")
(ql:quickload "unix-opts")
(ql:quickload "fset")
(ql:quickload "spinneret")
(require "spinneret")
(use-package :spinneret)

;; DEFINE COMMAND ARGUMENTS

(opts:define-opts
  (:name :help
   :description "Some help here needed." ; TODO manage program arguments.
   :short #\h
   :long "help"))

;; TODO
;; The parameters that we'd be interested for are the following:
;;  - the language of the page... although we could as well generate as many html files that
;;    we have from languages
;;  - the path of the output directory to which the html files should be created

;; SETUP LOCALIZATION

(defun read-lang-lisp (file-path)
  (with-open-file (in file-path)
    (with-standard-io-syntax
      (read in))))

(defparameter lang (read-lang-lisp "/home/davd/clisp/be-it/src/lang.en.lisp"))

(defun lang-get (key)
  "Get the translation for the given key."
  (getf lang key))

;; DEFINE WEB PAGE COMPONENTS

(defparameter *page-title* "Davd Rueda")

(defmacro concat (&body body)
  `(concatenate 'string ,@body))

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
       ,@body))))

(deftag link (text attrs &key href class)
  `(:a.contact-link
    :class ,class
    :href ,href
    ,@text))

(deftag work-experience (body attrs &key title duration desc technologies ref)
  `(:div.card
    (:h1 ,title
         (when ref (link :class "work-reference" :href ref "SEE REFERENCE")))
    (:em ,duration)
    (:p ,desc)
    (:div.card-tags
     (loop for tech in ,technologies
           collect (:div.card-tag tech)))
    ,@body))

(deftag repeat (template attrs &key for-lang)
  "This is a tag that repeats a given template using the key
   for a translation split into a list of several strings.
     - lang-binding-form: 2 elements list with var name and translation key
     - template: a single form (one list of potentially embedded tags)"
  (let ((lang-var-name (first for-lang))
        (lang-key (second for-lang)))
    (reduce #'(lambda (tag-product translation-value)
                (append
                 tag-product
                 `((let ((,lang-var-name ,translation-value))
                     ,@template))))
            (lang-get lang-key)
            :initial-value `(progn))))

(defun index ()
  (with-page (:title *page-title*)
    (:section.contact                   ; CONTACT & LANG
     (let ((my-mail (lang-get :contact.mail)))
       (link :href (concat "mailto:" my-mail) my-mail))
     (link :href (lang-get :contact.github) "Github")
     (link :href (lang-get :contact.linkedin) "Linkedin")
     (link :href (lang-get :contact.fork-project) "Fork me!")
     (:section.lang-flags
      (:em "Speaks: Fr / En / Sp / De")))
    (:header.centered                   ; CV TITLE - MY NAME BASICALLY...
     (:img
      :class "cv-img"
      :src "./resources/images/my.jpg"
      :alt (lang-get :cv.pic.img.alt))
     (:h1 (lang-get :cv.title))
     (:h2 (lang-get :cv.sub-title))
     (:section                          ; ABOUT ME
      (repeat
        :for-lang (about-me :about.me.txt.p)
        (:p.about-me about-me))))
    (:h1.work-exp-section-title "Work Experiences")
    (:section.work-exp-cards            ; WORK EXPERIENCE
     (repeat
       :for-lang (my-exps :work.experience)
       (destructuring-bind (&key title ref company desc duration technologies)
           my-exps
         (work-experience
           :title title
           :ref ref
           :company company
           :duration duration
           :desc desc
           :technologies technologies))))))

;; WRITES CV TO HTML FILE
(let ((linode-html-file-path "/mnt/linode/my/var/www/localhost/htdocs/index.html")
      (project-html-file-path "/home/davd/clisp/be-it/src/my-cv.html"))
  (with-open-file (cv-file project-html-file-path :direction :output
                                                  :if-exists :supersede)
    (let ((*html* cv-file))
      (index))))

;; exit program - that doesn't work yet... TODO
;; (sb-ext:exit)
