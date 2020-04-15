(in-package #:dao)

(defparameter *connection* nil)
(defun connect (&optional (db-name "be_it") (username "postgres") (password "helloworld"))
  (when (null *connection*)
    (setf *connection*
          (mito:connect-toplevel :postgres
                                 :database-name db-name
                                 :username username
                                 :password password))))

;; TABLE DEFINITIONS
;; Contact information
(mito:deftable contact ()
  ((mail :col-type (:varchar 255))
   (linkedin :col-type (:varchar 255))
   (github :col-type (:varchar 255))))

;; The whole CV
(mito:deftable cv ()
  ((title :col-type (:varchar 255))
   (sub-title :col-type (:varchar 255))
   (image-description :col-type (:varchar 255))
   (contact :col-type (or contact :null) :references contact)))

;; What books I've been reading
(mito:deftable reading ()
  ((title :col-type (:varchar 255))
   (image :col-type (:varchar 255))
   (external-url :col-type (:varchar 255))
   (cv :col-type (or cv :null) :references cv)))

;; The professional experience
(mito:deftable work-experience ()
  ((title :col-type (:varchar 255))
   (company :col-type (or (:varchar 255) :null))
   (description :col-type (:varchar 255))
   (technologies :col-type (:varchar 255))
   (remote :col-type (or (:char 1) :null))
   (cv :col-type (or cv :null) :references cv)))

;; A paragraph is a list of elements (e.g. text, links)
(mito:deftable paragraph-element ()
  ((section :col-type (:varchar 255))
   (paragraph :col-type (:varchar 255))
   (order :col-type :real)
   (content :col-type :text)
   (cv :col-type (or cv :null) :references cv))
  (primary-key section paragraph))

;; Mappers
(defmacro make-mapper (kind mappings)
  "Return a function of a mapper."
  `(let ((map (make-hash-table)))
     ,@(reduce #'(lambda (acc curr) (append acc `((hm:put map ,@curr))))
               mappings
               :initial-value `())
     (make-json->dao-mapper :hm map
                            :kind ,kind)))

(defstruct json->dao-mapper
  hm
  kind)

;; MAPPERS
(defun contact-mapper ()
  "Define a contact mapper that maps a contact JSON object to a dao:contact."
  (make-mapper 'contact
               (('mail :mail)
                ('linkedin :linkedin)
                ('github :github))))

(defun cv-mapper ()
  "Define a cv mapper that maps cv JSON object to a dao:cv."
  (make-mapper 'cv
               (('title :title)
                ('sub-title :sub-title)
                ('image-description :image-description)
                ('contact :contact))))

(defun reading-mapper ()
  "Define a reading mapper that maps reading JSON object to a dao:reading."
  (make-mapper 'reading
               (('title :title)
                ('image :image)
                ('external-url :external-url)
                ('cv :cv))))

(defun work-experience-mapper ()
  "Define a work-experience mapper that maps work-experience JSON object to a dao:work-experience."
  (make-mapper 'work-experience
               (('title :title)
                ('company :company)
                ('description :description)
                ('technologies :technologies)
                ('cv :cv))))

(defun paragraph-element-mapper ()
  "Define a paragraph-element mapper that maps paragraph-element JSON object to a dao:paragraph-element."
  (make-mapper 'paragraph-element
               (('section :section)
                ('paragraph :paragraph)
                ('order :order)
                ('content :content)
                ('cv :cv))))

;; INSERT FROM JSON
(defun insert-paragraph-element (json-paragraph-element)
  "Insert paragraph-element from json object."
  (mito:insert-dao (json->dao (paragraph-element-mapper) json-paragraph-element)))

(defun insert-work-experience (json-work-experience)
  "Insert work-experience from json object."
  (mito:insert-dao (json->dao (work-experience-mapper) json-work-experience)))

(defun insert-contact (json-contact-obj)
  "Insert contact from json object."
  (mito:insert-dao (json->dao (contact-mapper) json-contact-obj)))

(defun insert-cv (json-cv-obj)
  "Insert cv from json object."
  (mito:insert-dao (json->dao (cv-mapper) json-cv-obj)))

(defun insert-reading (json-reading-obj)
  "Insert reading json object."
  (mito:insert-dao (json->dao (reading-mapper) json-reading-obj)))

;; MAP A JSON TO A DAO
(defun json->dao (mapper json)
  "Fills the dao from the given JSON object and according to the mapper."
  (let* ((m-kind (json->dao-mapper-kind mapper))
         (dao (make-instance m-kind)))
    (maphash #'(lambda (k v)
                 (setf (slot-value dao k) (jsons:get-in json v)))
             (json->dao-mapper-hm mapper))
    dao))

;; CREATE/DROP TABLES
(defun create-table (table-type)
  "Creates the table of given type."
  (restart-case
      (when (not (mito.db:table-exists-p *connection*
                                         (mito.class:table-name (find-class table-type))))
        (format t "~&CREATE TABLE: ~A" table-type)
        (mapc #'mito:execute-sql (mito:table-definition table-type))
        (mito:ensure-table-exists table-type)
        t)
    (skip () nil)))

(defun drop-table (table-type)
  "Drops table of given type."
  (restart-case
      (when (mito.db:table-exists-p *connection*
                                    (mito.class:table-name (find-class table-type)))
        (format t "~&DROP TABLE: ~A" table-type)
        (mito:execute-sql (sxql:drop-table table-type))
        t)
    (skip () nil)))

(defparameter all-tables '(contact
                           paragraph-element
                           reading
                           work-experience
                           cv))

(defun create-tables ()
  (mapcar #'create-table all-tables))

(defun drop-tables ()
  (mapcar #'drop-table all-tables))

(defun reset-db-tables ()
  (drop-tables)
  (create-tables))

;;; RETRIEVE CV
(defun retrieve-cv (cv-id)
  (mito:select-dao 'cv
    (mito:includes 'contact)
    (sxql:where (:= :id cv-id))
    (sxql:limit 1)))

(defun retrieve-cv-by-title (cv-title)
  (mito:select-dao 'cv
    (mito:includes 'contact)
    (sxql:where (:like :title cv-title))))

(defun retrieve-readings (cv-id)
  (mito:select-dao 'reading
    (sxql:where (:= :cv_id cv-id))))

(defun retrieve-work-experiences (cv-id)
  (mito:select-dao 'work-experience
    (sxql:where (:= :cv_id cv-id))))

(defun retrieve-paragraph-elements (cv-id)
  (mito:select-dao 'paragraph-element
    (sxql:where (:= :cv_id cv-id))))
