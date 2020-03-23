(in-package #:dao)

(defparameter *connection* nil)
(defun connect ()
  (when (null *connection*)
    (setf *connection*
          (mito:connect-toplevel :postgres
                                 :database-name "be_it"
                                 :username "postgres"
                                 :password "helloworld"))))

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
   (contact :col-type (or contact :null) :references contact))
  (:unique-keys title))

;; What books I've been reading
(mito:deftable reading ()
  ((title :col-type (:varchar 255))
   (image :col-type (:varchar 255))
   (external-url :col-type (:varchar 255))
   (cv :col-type (or cv :null) :references cv)))

;; The professional experience
(mito:deftable work-experience ()
  ((title :col-type (:varchar 255))
   (company :col-type (:varchar 255))
   (description :col-type (:varchar 255))
   (technologies :col-type (:varchar 255))
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
     ,@(reduce #'(lambda (acc curr) (append acc `((hm:hm-put map ,@curr))))
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

;; UTILITY
;; (defun cv-obj? (cv)
;;   "Return T if cv is a cv object."
;;   (eq 'cv (type-of cv)))

;; (defun not-cv-obj? (cv)
;;   "Complements cv-obj?"
;;   (complement #'cv-obj?))

;; (defun print-cv (cv)
;;   (format t "~&++ CV ++")
;;   (format t "~& title = ~A" (slot-value cv 'title))
;;   (format t "~& sub-title = ~A" (slot-value cv 'sub-title))
;;   (format t "~& image-descrigtion = ~A" (slot-value cv 'image-description)))

;; CREATE TABLES
(defun create-table (table-type)
  "Creates the table as defined in the mito:deftable call of the symbol table-type."
  (mapc #'mito:execute-sql (mito:table-definition table-type))
  (mito:ensure-table-exists table-type))

(defun create-tables ()
  (create-table 'contact)
  (create-table 'paragraph-element)
  (create-table 'reading)
  (create-table 'work-experience)
  (create-table 'cv))

;; some testing
(defun select-examples ()

  ;; some manual queries
  (mito:execute-sql (select :*
                      (from :pca)))

  ;; some dao queries
  (let ((bx (mito:find-dao 'pca :city "Bordeaux")))
    (format t "~&city = ~A" (slot-value bx 'city)))

  (let ((all-data (mito:retrieve-dao 'pca)))
    (when (listp all-data) (format t "received a list"))
    (loop
       for city in all-data
       do (print-pca city)))
  )
