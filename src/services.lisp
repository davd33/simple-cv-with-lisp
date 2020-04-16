(in-package #:services)

(defmacro defpost (name (&rest args) (&rest json-args) doc &body body)
  "Creates a function that contains code to validate that every JSON-ARGS
given to a function call is checked against the desired class.

The JSON-ARGS arguments shall be lists of two elements:
 - the name of the variable
 - type to which the json should be compatible with.

Example:
\(defpost hello \(a b c\) \(\(j1 'class-type\) \(j2 'class-type\)\)
  ...docstring...
  ...body using a, b, c, j1, and j2...\)"
  `(defun ,name ,(append args (mapcar #'first json-args))
     ,(str:concat doc)
     ,@(reduce #'(lambda (acc j-a) (append acc `((assert (listp ,(first j-a)))
                                                 (assert (jsons:type-compatible-p ,@j-a)))))
               json-args
               :initial-value (list))
     ,@body))

(defun get-cv (cv-id)
  "Retrieves CV in DB by title."
  (handler-case (let* ((cv-dao (first (dao:retrieve-cv cv-id)))

                       (work-experience-dto (mapcar (mop:make-mapper
                                                         dao:work-experience
                                                         dto:work-experience-dto)
                                                     (dao:retrieve-work-experiences cv-id)))

                       (reading-dto (mapcar (mop:make-mapper
                                                 dao:reading
                                                 dto:reading-dto)
                                             (dao:retrieve-readings cv-id)))

                       (section-dto
                        ;; WE GROUP BY SECTION THE RESULT OF RETRIEVING PARAGRAPH-ELEMENTS FROM THE DB
                        (let ((grouped-by-section (data:group-by (dao:retrieve-paragraph-elements cv-id)
                                                                 :kv-pair #'(lambda (x)
                                                                              (list (slot-value x 'dao:section) x)))))
                          (hm:reduce #'(lambda (sections k p-elts-dao)
                                         (cons (let ((section (make-instance 'dto:section-dto))
                                                     (section-title (slot-value (first p-elts-dao) 'dao:section))
                                                     ;; WE GROUP BY PARAGRAPH NAME
                                                     (grouped-by-p (data:group-by p-elts-dao
                                                                                  :kv-pair #'(lambda (x)
                                                                                               (list (slot-value x 'dao:paragraph) x)))))
                                                 (setf (dto:title section) section-title)
                                                 (setf (dto:paragraphs section)
                                                       (hm:reduce #'(lambda (paragraphs k p-elts-dao2)
                                                                      (cons (let ((paragraph (make-instance 'dto:paragraph-dto)))
                                                                              (setf (dto:elements paragraph)
                                                                                    (mapcar (mop:make-mapper
                                                                                                dao:paragraph-element
                                                                                                dto:paragraph-element-dto)
                                                                                            p-elts-dao2))
                                                                              paragraph)
                                                                            paragraphs))
                                                                  grouped-by-p
                                                                  (list)))
                                                 section)
                                               sections))
                                     grouped-by-section
                                     (list))))

                       ;; BUILD CV DTO
                       (cv-dto (funcall (mop:make-mapper
                                            dao:cv
                                            dto:cv-dto
                                          (mop:with-mapped-slot 'dao:contact 'dto:contact (mop:make-mapper dao:contact dto:contact-dto))
                                          (mop:with-computed-slot 'dto:work-experiences work-experience-dto)
                                          (mop:with-computed-slot 'dto:readings reading-dto)
                                          (mop:with-computed-slot 'dto:sections section-dto))
                                        cv-dao)))

                  cv-dto)

    (dbi.error:<dbi-database-error> (e)
      (format t "~&error while retrieving CV '~A': ~A" cv-id e)
      (format nil "ERROR DB: ~A" e))))

(defpost store-cv () ((contact-json 'dto:contact-dto)
                      (readings-json 'dto:reading-dto t)
                      (work-experiences-json 'dto:work-experience-dto t)
                      (section-list-json 'dto:section-dto t)
                      (cv-json 'dto:cv-dto))
    "Store a CV in DB."
  (handler-case (let* ((contact (dao:insert-contact contact-json))

                       (cv (dao:insert-cv (acons :contact contact cv-json)))

                       (readings (loop for r in readings-json
                                    collect
                                      (dao:insert-reading (acons :cv cv r))))

                       (work-experiences (loop for we in work-experiences-json
                                            collect
                                              (dao:insert-work-experience
                                               (alists:aconses we `(:cv ,cv)))))

                       (paragraph-elements (loop for section in section-list-json
                                              append
                                                (let ((section-title (get-in section :title)))
                                                  (loop for p in (get-in section :paragraph-list)
                                                     append
                                                       (let ((p-title (get-in p :title)))
                                                         (loop for p-elt in (get-in p :element-list)
                                                            collect
                                                              (dao:insert-paragraph-element (alists:aconses p-elt
                                                                                                            `(:section ,section-title)
                                                                                                            `(:paragraph ,p-title)
                                                                                                            `(:content ,(json:encode-json-to-string
                                                                                                                         (get-in p-elt :content)))
                                                                                                            `(:cv ,cv)))))))))

                       (response (acons :command "create-cv" nil))

                       (response (jsons:add-value (length readings) response
                                                  :created :readings))

                       (response (jsons:add-value (length work-experiences) response
                                                  :created :work-experiences))

                       (response (jsons:add-value (length paragraph-elements) response
                                                  :created :paragraphs-elements))
                       (response (jsons:add-value (slot-value cv 'mito.dao.mixin::id)
                                                  response
                                                  :created :cv-id)))

                  (json:encode-json-alist-to-string response))
    (dbi.error:<dbi-database-error> (e)
      (format t "~&error during CV creation: ~A" e)
      (format nil "ERROR DB: ~A" e))))
