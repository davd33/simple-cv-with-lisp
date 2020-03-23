(defpackage #:jsons
  (:use #:cl)
  (:export #:get-in))

(defpackage #:hm
  (:use #:cl)
  (:export #:hm-put
           #:hm-get))

(defpackage #:alists
  (:use #:cl)
  (:export #:aconses))

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
   #:insert-contact
   #:insert-cv
   #:insert-reading
   #:insert-work-experience
   #:insert-paragraph-element
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
