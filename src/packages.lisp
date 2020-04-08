(defpackage #:alists
  (:use #:cl #:alexandria)
  (:export #:aconses
           #:deep-acons
           #:merge-acons))

(defpackage #:jsons
  (:use #:cl)
  (:export #:get-in
           #:add-value))

(defpackage #:hm
  (:use #:cl)
  (:shadow #:get
           #:reduce
           #:first)
  (:export #:put
           #:get
           #:one
           #:reduce
           #:print-elt
           #:print-all))

(defpackage #:data
  (:use #:cl #:alexandria)
  (:export #:group-by))

(defpackage #:memoize
  (:use #:cl #:alexandria)
  (:export #:memo
           #:memoize
           #:clear-memoize
           #:defmemo))

(defpackage #:pipe
  (:use #:cl)
  (:export #:delay
           #:force
           #:make-pipe
           #:empty-pipe
           #:head
           #:tail
           #:pipe-elt
           #:integers
           #:foreach))

(defpackage #:mop
  (:use #:cl #:alexandria)
  (:export #:make-mapper
           #:defprintobj
           #:with-computed-slot
           #:with-renamed-slot))

(defpackage #:dao
  (:use #:cl #:sxql)
  (:export
   ;; ENTITIES
   #:contact
   #:work-experience
   #:reading
   #:paragraph-element
   #:cv
   ;; ENTITY FIELDS
   #:title
   #:sub-title
   #:mail
   #:id
   #:section
   #:paragraph
   ;; RETRIEVE
   #:retrieve-cv
   #:retrieve-readings
   #:retrieve-work-experiences
   #:retrieve-paragraph-elements
   ;; INSERT
   #:insert-contact
   #:insert-cv
   #:insert-reading
   #:insert-work-experience
   #:insert-paragraph-element
   ;; CONNECT AND CREATE TABLES
   #:connect
   #:create-tables
   #:*connection*))

(defpackage #:dto
  (:use #:cl)
  (:export #:contact-dto
           #:work-experience-dto
           #:reading-dto
           #:paragraph-element-dto
           #:paragraph-dto
           #:section-dto
           #:cv-dto
           ;; ACCESSORS
           #:title
           #:sub-title
           #:image
           #:image-description
           #:external-url
           #:order
           #:contact
           #:company
           #:description
           #:duration
           #:technologies
           #:sections
           #:paragraphs
           #:elements
           #:content
           #:linkedin
           #:mail
           #:github
           #:work-experiences
           #:readings))

(defpackage #:api
  (:use #:cl #:snooze #:jsons #:alexandria)
  (:export #:start
           #:stop))

(defpackage #:be-it
  (:use #:cl #:spinneret)
  (:export #:save
           #:index
           #:with-page
           #:cv->html))

(defpackage #:dev.mocks
  (:use #:cl)
  (:export #:api-cv
           #:home))

(defpackage #:web-site
  (:use #:cl #:snooze #:jsons #:alexandria #:spinneret)
  (:export #:start-all
           #:stop-all
           #:home))

(defpackage #:services
  (:use #:cl #:jsons #:alexandria #:spinneret)
  (:export #:store-cv
           #:get-cv))
