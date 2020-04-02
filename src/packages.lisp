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
  (:export #:hm-put
           #:hm-get))

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

(defpackage #:clos-mapping
  (:use #:cl #:alexandria)
  (:export #:make-mapper
           #:defprintobj))

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

(defpackage #:api-dtos
  (:use #:cl)
  (:export #:contact-dto
           #:work-experience-dto
           #:reading-dto
           #:paragraph-element-dto
           #:paragraph-dto
           #:section-dto
           #:cv-dto
           ;; FIELDS
           #:linkedin
           #:mail))

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
  (:use #:cl #:jsons #:alexandria #:spinneret #:api-dtos)
  (:export #:store-cv
           #:get-cv))
