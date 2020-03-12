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
