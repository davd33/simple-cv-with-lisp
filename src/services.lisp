(in-package #:services)

(defun get-cv (cv-title)
  "Retrieves CV in DB by title."
  (handler-case (let* ((cv-dao (first (dao:retrieve-cv cv-title)))
                       (cv-id (slot-value cv-dao 'mito.dao.mixin::id))

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
      (format t "~&error while retrieving CV '~A': ~A" cv-title e)
      (format nil "ERROR DB: ~A" e))))

(defun store-cv (contact-json
                 readings-json
                 work-experiences-json
                 paragraphs-json
                 cv-json)
  "Store a CV in DB."
  (handler-case (let* ((contact (dao:insert-contact contact-json))

                       (cv (dao:insert-cv (acons :contact contact cv-json)))

                       (readings (loop for r in readings-json
                                    collect
                                      (dao:insert-reading (acons :cv cv r))))

                       (work-experiences (loop for we in work-experiences-json
                                            collect
                                              (dao:insert-work-experience
                                               (alists:aconses we
                                                               `(:technologies
                                                                 ,(format nil
                                                                          "~{~A~^,~}"
                                                                          (get-in we :technologies)))
                                                               `(:cv ,cv)))))
                       (paragraph-elements (loop for p in paragraphs-json
                                              collect
                                                (loop for p-elt in (get-in p :elements)
                                                   collect
                                                     (dao:insert-paragraph-element
                                                      (alists:aconses p-elt
                                                                      `(:section ,(get-in p :section))
                                                                      `(:content ,(json:encode-json-to-string
                                                                                   (get-in p-elt :content)))
                                                                      `(:cv ,cv)))))))
                  (let* ((response (acons :command "Create CV." nil))
                         (response (when readings
                                     (jsons:add-value (length readings) response
                                                      :created :readings)))
                         (response (when work-experiences
                                     (jsons:add-value (length work-experiences) response
                                                      :created :work-experiences)))
                         (response (when paragraph-elements
                                     (jsons:add-value (length paragraph-elements) response
                                                      :created :paragraphs-elements)))
                         (response (when cv
                                     (jsons:add-value "CV Stored" response
                                                      :created :cv))))
                    (json:encode-json-alist-to-string response)))
    (dbi.error:<dbi-database-error> (e)
      (format t "~&error during CV creation: ~A" e)
      (format nil "ERROR DB: ~A" e))))
