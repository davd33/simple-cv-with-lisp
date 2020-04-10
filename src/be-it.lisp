(in-package :be-it)

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

(eval-when
    (:compile-toplevel
     :load-toplevel
     :execute)
  (defun read-lang-lisp (file-path)
    (with-open-file (in file-path)
      (with-standard-io-syntax
        (read in)))))

(eval-when
    (:compile-toplevel
     :load-toplevel
     :execute)
  (defparameter *lang* (read-lang-lisp "/home/davd/clisp/be-it/src/lang.en.lisp")))

(eval-when
    (:compile-toplevel
     :load-toplevel
     :execute)
  (defun lang-get (key)
    "Get the translation for the given key."
    (getf *lang* key)))

;; DEFINE WEB PAGE COMPONENTS

(defparameter *page-title* "Davd Rueda")

(defmacro css (&body styles)
  "Takes 1..n CSS instructions as 2-elements lists, then returns a css formatted string.
   A CSS instruction list looks like this: (:font-size <string>)"
  `(str:concat
     ,@(loop for style in styles
          collect `(format nil "~a: ~a;~%" ,(string (first style)) ,(second style)))))

(defmacro with-page ((&key title) &body body)
  `(spinneret:with-html
     (:doctype)
     (:html
      (:head
       (:link :href "/css/cv.css" :rel "stylesheet" :type "text/css")
       (:link :href "/css/font-awesome.css" :rel "stylesheet" :type "text/css")
       (:title ,title))
      (:body
       ,@body))))

(deftag link (text attrs &key href class)
  `(:a.contact-link
    :class ,class
    :href ,href
    ,@attrs
    ,@text))

(deftag reading (body attrs &key reading)
  "Displays a DTO:READING-DTO."
  `(with-slots ((title dto:title)
                (image-path dto:image)
                (external-url dto:external-url)) ,reading

     (:a.book-card
      :style (css
               (:width "200px")
               (:margin "10px")
               (:text-align :center)
               (:color "#222")
               (:text-decoration :none))
      :href (or external-url "#")
      :title title
      (:img :width 130
            :style (css
                     (:margin "0 auto")
                     (:background :darkblue))
            :src (str:concat "/images/" image-path)))))

(deftag paragraph (body attrs &key paragraph-dto black-mode)
  "Displays a DTO:PARAGRAPH-DTO."
  `(with-slots ((pels dto:elements)) ,paragraph-dto
     (reduce
      #'(lambda (a pel)
          (let ((content (handler-case (json:decode-json-from-string (dto:content pel))
                           (json:json-syntax-error () (dto:content pel)))))
            (cons (if (listp content)
                      (cond
                        ((equalp :link (first content)) (link
                                                          :style (css (:margin "0"))
                                                          :href (third content)
                                                          (second content))))
                      (:span content))
                  a)))
      (sort pels #'< :key #'dto:order)
      :initial-value (:p))))

(deftag work-experience (body attrs &key work-experience)
  "Displays a work experience."
  `(with-slots ((title dto:title)
                (company dto:company)
                (desc dto:description)
                ;; (duration dto:duration)
                ;; (remote? dto:remote?)
                ;; (ref dto:reference)
                (technologies dto:technologies)) ,work-experience

     (let ((technologies (str:split "," technologies)))

       (:div.card
        ;; (when remote? (:i :style (css (:float :right))
        ;;                   :title "remote position"
        ;;                   :class "fal fa-wifi"))
        (:h1 title
             ;; (when ,ref (link :class "work-reference" :href ref "SEE REFERENCE"))
             )
        (:h4 company)
        ;; (:em duration)
        (:p desc)
        (:div.card-tags
         (loop for tech in technologies
            collect (:div.card-tag tech)))
        ,@body))))

(deftag repeat (template attrs &key for)
  "This is a tag that repeats a given template using the key
for a translation split into a list of several strings.
  - for: lang-binding-form: 2 elements list with var name and translation key
  - template: a single form (one list of potentially embedded tags)"
  `(reduce #'(lambda (acc elt)
               (append
                acc
                (let ((,(caadr for) elt))
                  ,@template)))
           ,@(cdadr for)
           :initial-value `(progn)))

(defun cv->html (cv-title cv)
  (with-page (:title cv-title)
    (labels ((paragraphs-by-section-title (sections title)
               "Find a DTO:SECTION-DTO in a list of sections by DTO:TITLE."
               (dto:paragraphs (find title sections
                                     :key #'dto:title :test #'string=))))

      ;; TOP BAND INFORMATION
      (with-slots ((co-mail dto:mail)
                   (co-github dto:github)
                   (co-linkedin dto:linkedin)) (dto:contact cv)

        (:section.contact
         (link :href (str:concat "mailto:" co-mail) co-mail)
         (link :href co-github "Github")
         (link :href co-linkedin "Linkedin")
         (link :href "https://github.com/davd33/simple-cv-with-lisp" "(fork-me!)")
         (:span :class "pdf-download-link"
                (link :href "/pdf/cv.david-rueda.pdf" "PDF"))
         (:section.lang-flags
          (:em "Speaks: Fr / En / Sp / De"))))

      ;; CV TITLE - IMAGE - INTRODUCTION
      (:header.centered
       (:img
        :class "cv-img"
        :src "/images/my.jpg"
        :alt (dto:image-description cv))
       (:h1 cv-title)
       (:h2 (dto:sub-title cv))
       (:section
        (repeat
          :for (about-me (paragraphs-by-section-title (dto:sections cv) "about.me.txt.p"))
          (paragraph :paragraph-dto about-me))))

      ;; WORK EXPERIENCE
      (:h1.centered.dark-title "Work Experiences")
      (:section.work-exp-cards
       (repeat
         :for (we (dto:work-experiences cv))
         (work-experience :work-experience we)))

      ;; BOOKS THAT I READ
      (:h1.centered "Reading")
      (:section.books :id "books-section"
                      (:div.books
                       :style (css
                                (:display :flex)
                                (:margin-bottom "50px")
                                (:flex-wrap :wrap)
                                (:justify-content :center)
                                (:align-items :baseline))
                       (repeat :for (reading (dto:readings cv))
                               (reading :reading reading))))

      ;; LISP EXPERIENCE
      (:h1.centered.dark-title "When I discovered Lisp")
      (:section.lisp-experience
       (repeat :for (lisp (paragraphs-by-section-title (dto:sections cv)
                                                       "my-experience-with-lisp"))
               (paragraph :paragraph-dto lisp))))))
