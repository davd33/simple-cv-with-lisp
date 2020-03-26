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
  (:use #:cl #:snooze #:jsons #:alexandria)
  (:export #:start
           #:stop))

(defpackage #:be-it
  (:use #:cl)
  (:export #:save)
  (:shadowing-import-from #:spinneret))

(defpackage #:dev.mocks
  (:use #:cl))

(defpackage #:web-site
  (:use #:cl #:snooze #:jsons #:alexandria #:spinneret)
  (:export #:start-all
           #:stop-all))
