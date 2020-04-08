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

(deftag reading (body attrs &key title image-path ext-link)
  `(:a.book-card
    :style (css
             (:width "200px")
             (:margin "10px")
             (:text-align :center)
             (:color "#222")
             (:text-decoration :none))
    :href (or ext-link "#")
    :title ,title
    (:img :width 130
          :style (css
                   (:margin "0 auto")
                   (:background :darkblue))
          :src (str:concat "/images/" ,image-path))))

(deftag paragraph (body attrs &key paragraph-dto)
  "Displays a DTO:PARAGRAPH-DTO."
  `(with-slots ((pels dto:elements)) ,paragraph-dto
     (reduce
      #'(lambda (a pel)
          (cons (:span (dto:content pel))
                a))
      (sort pels #'< :key #'dto:order)
      :initial-value (:p))))

(deftag work-experience (body attrs &key title duration desc technologies ref company remote?)
  "Displays a work experience."
  `(:div.card
    (when remote? (:i :style (css (:float :right)) :title "remote position" :class "fal fa-wifi"))
    (:h1 ,title
         (when ,ref (link :class "work-reference" :href ref "SEE REFERENCE")))
    (:h4 ,company)
    (:em ,duration)
    (:p ,desc)
    (:div.card-tags
     (loop for tech in ,technologies
           collect (:div.card-tag tech)))
    ,@body))

(deftag repeat (template attrs &key for-lang direct-mode)
  "This is a tag that repeats a given template using the key
for a translation split into a list of several strings.
  - for-lang: lang-binding-form: 2 elements list with var name and translation key
  - template: a single form (one list of potentially embedded tags)
  - direct-mode: when not nil, for-lang designate the value to iterate over"
  (let ((var-name (first for-lang))
        (lang-key-or-value (second for-lang)))

    (reduce #'(lambda (tag-product translation-value)
                (append
                 tag-product
                 `((let ((,var-name ,translation-value))
                     ,@template))))
            (if (null direct-mode)
                (lang-get lang-key-or-value)
                lang-key-or-value)
            :initial-value `(progn))))

(defun index ()
  (with-page (:title *page-title*)
    (:section.contact                   ; CONTACT & LANG
     (let ((my-mail (lang-get :contact.mail)))
       (link :href (str:concat "mailto:" my-mail) my-mail))
     (link :href (lang-get :contact.github) "Github")
     (link :href (lang-get :contact.linkedin) "Linkedin")
     (link :href (lang-get :contact.fork-project) "(fork-me!)")
     (:span :class "pdf-download-link"
            (link :href "/pdf/cv.david-rueda.pdf" "PDF"))
     (:section.lang-flags
      (:em "Speaks: Fr / En / Sp / De")))
    (:header.centered                   ; CV TITLE - MY NAME BASICALLY...
     (:img
      :class "cv-img"
      :src "/images/my.jpg"
      :alt (lang-get :cv.pic.img.alt))
     (:h1 (lang-get :cv.title))
     (:h2 (lang-get :cv.sub-title))
     (:section                          ; ABOUT ME
      (repeat
        :for-lang (about-me :about.me.txt.p)
        (:p.about-me about-me))))
    (:h1.centered.dark-title "Work Experiences")
    (:section.work-exp-cards            ; WORK EXPERIENCE
     (repeat
       :for-lang (my-exps :work.experience)
       (destructuring-bind (&key title ref company desc duration technologies remote?)
           my-exps
         (work-experience
           :title title
           :ref ref
           :company company
           :duration duration
           :desc desc
           :technologies technologies))))
    (:h1.centered "Reading")
    (:section.books :id "books-section" ; BOOKS THAT I READ
                    (:div.books
                     :style (css
                              (:display :flex)
                              (:margin-bottom "50px")
                              (:flex-wrap :wrap)
                              (:justify-content :center)
                              (:align-items :baseline))
                     (repeat :for-lang (books-read :reading)
                       (destructuring-bind (&key title image-path ext-link)
                           books-read
                         (reading :title title
                           :ext-link ext-link
                           :image-path image-path)))))
    (:h1.centered.dark-title "When I discovered Lisp")
    (:section.lisp-experience           ; LISP EXPERIENCE
     (repeat :for-lang (paragraph :my-experience-with-lisp)
       (reduce #'(lambda (acc curr)     ; TODO create a function
                   (cons (or (progn
                               (when (listp curr)
                                 (cond
                                   ((equalp :link (first curr)) (link
                                                                  :style (css (:margin "0"))
                                                                  :href (third curr)
                                                                  (second curr))))))
                             (:span curr))
                         acc))
               paragraph
               :initial-value (:p))))))

(defun cv->html (cv-name)
  (labels ((paragraphs-by-section-title (sections title)
             "Find a DTO:SECTION-DTO in a list of sections by DTO:TITLE."
             (dto:paragraphs (find title sections
                                   :key #'dto:title :test #'string=))))

    (with-slots ((cv-title dto:title)
                 (cv-sub-title dto:sub-title)
                 (cv-img-desc dto:image-description)
                 (cv-contact dto:contact)
                 (cv-sections dto:sections)
                 (cv-we dto:work-experiences)) (services:get-cv cv-name)

      (with-slots ((we-title dto:title)
                   (we-company dto:company)
                   (we-description dto:description)
                   (we-duration dto:duration)
                   (we-technologies dto:technologies)) cv-we

        (with-slots ((co-mail dto:mail)
                     (co-github dto:github)
                     (co-linkedin dto:linkedin)) cv-contact

          (with-page (:title cv-title)

            ;; CONTACT & LANG
            (:section.contact
             (link :href (str:concat "mailto:" co-mail) co-mail)
             (link :href co-github "Github")
             (link :href co-linkedin "Linkedin")
             (link :href "https://github.com/davd33/simple-cv-with-lisp" "(fork-me!)")
             (:span :class "pdf-download-link"
                    (link :href "/pdf/cv.david-rueda.pdf" "PDF"))
             (:section.lang-flags
              (:em "Speaks: Fr / En / Sp / De")))

            ;; CV TITLE - MY NAME BASICALLY...
            (:header.centered
             (:img
              :class "cv-img"
              :src "/images/my.jpg"
              :alt cv-img-desc)
             (:h1 cv-title)
             (:h2 cv-sub-title)

             ;; ABOUT ME
             (:section
              (repeat
                :for-lang (about-me (paragraphs-by-section-title sections "about.me.txt.p"))
                (paragraph :paragraph-dto about-me))))
            (:h1.centered.dark-title "Work Experiences")
            (:section.work-exp-cards      ; WORK EXPERIENCE
             (repeat
               :for-lang (my-exps :work.experience)
               (destructuring-bind (&key title ref company desc duration technologies remote?)
                   my-exps
                 (work-experience
                   :title title
                   :ref ref
                   :company company
                   :duration duration
                   :desc desc
                   :technologies technologies))))
            (:h1.centered "Reading")
            (:section.books :id "books-section" ; BOOKS THAT I READ
                            (:div.books
                             :style (css
                                      (:display :flex)
                                      (:margin-bottom "50px")
                                      (:flex-wrap :wrap)
                                      (:justify-content :center)
                                      (:align-items :baseline))
                             (repeat :for-lang (books-read :reading)
                                     (destructuring-bind (&key title image-path ext-link)
                                         books-read
                                       (reading :title title
                                                :ext-link ext-link
                                                :image-path image-path)))))
            (:h1.centered.dark-title "When I discovered Lisp")
            (:section.lisp-experience     ; LISP EXPERIENCE
             (repeat :for-lang (paragraph :my-experience-with-lisp)
                     (reduce #'(lambda (acc curr) ; TODO create a function
                                 (cons (or (progn
                                             (when (listp curr)
                                               (cond
                                                 ((equalp :link (first curr)) (link
                                                                                :style (css (:margin "0"))
                                                                                :href (third curr)
                                                                                (second curr))))))
                                           (:span curr))
                                       acc))
                             paragraph
                             :initial-value (:p))))))))))

(defun save ()
  (let ((linode-html-file-path "/home/davd/linode/var/www/localhost/htdocs/index.html")
        (project-html-file-path "/home/davd/clisp/be-it/src/my-cv.html"))
    (with-open-file (cv-file linode-html-file-path :direction :output
                                                   :if-exists :supersede)
      (let ((spinneret:*html* cv-file))
        (index)))))
