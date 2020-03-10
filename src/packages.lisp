(defpackage #:dao
  (:use #:cl #:sxql)
  (:export #:contact
           #:work-experience
           #:reading
           #:paragraph-element
           #:cv
           #:title
           #:sub-title
           #:connect
           #:create-tables))

(defpackage #:api
  (:use #:cl #:snooze)
  (:export #:start
           #:stop))

(defpackage #:be-it
  (:use #:cl)
  (:export #:save)
  (:shadowing-import-from #:spinneret))
