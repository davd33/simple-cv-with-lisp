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
   ;; CONNECT AND CREATE TABLES
   #:connect
   #:create-tables
   #:*connection*))

(defpackage #:api
  (:use #:cl #:snooze)
  (:export #:start
           #:stop))

(defpackage #:be-it
  (:use #:cl)
  (:export #:save)
  (:shadowing-import-from #:spinneret))
