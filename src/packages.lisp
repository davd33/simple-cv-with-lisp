(defpackage #:jsons
  (:use #:cl)
  (:export #:get-in))

(defpackage #:hm
  (:use #:cl)
  (:export #:hm-put
           #:hm-get))

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
   ;; CREATE DAOs
   #:json->dao
   ;; JSON -> DAO MAPPERS
   #:contact-mapper
   ;; CONNECT AND CREATE TABLES
   #:connect
   #:create-tables
   #:*connection*))

(defpackage #:api
  (:use #:cl #:snooze #:jsons)
  (:export #:start
           #:stop))

(defpackage #:be-it
  (:use #:cl)
  (:export #:save)
  (:shadowing-import-from #:spinneret))
