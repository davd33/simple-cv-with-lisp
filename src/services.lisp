(in-package #:services)

(defun get-cv (cv-title)
  "Retrieves CV in DB by title."
  (handler-case (let* ((cv-dao (first (dao:retrieve-cv cv-title)))
                       (cv-id (slot-value cv-dao 'mito.dao.mixin::id))

                       (work-experience-dtos (mapcar (clos-mapping:make-mapper
                                                         dao:work-experience
                                                         api-dtos:work-experience-dto)
                                                     (dao:retrieve-work-experiences cv-id)))

                       (reading-dtos (mapcar (clos-mapping:make-mapper
                                                 dao:reading
                                                 api-dtos:reading-dto)
                                             (dao:retrieve-readings cv-id)))

                       (section-dtos
                        ;; WE GROUP BY SECTION THE RESULT OF RETRIEVING PARAGRAPH-ELEMENTS FROM THE DB
                        (let ((grouped-by-section (data:group-by (dao:retrieve-paragraph-elements cv-id)
                                                                 :kv-pair #'(lambda (x)
                                                                              (list (slot-value x 'dao:section) x)))))
                          (hm:reduce #'(lambda (sections k p-elts-dao)
                                         (cons (let ((section (make-instance 'api-dtos:section-dto))
                                                     (section-title (slot-value (first p-elts-dao) 'dao:section))
                                                     ;; WE GROUP BY PARAGRAPH NAME
                                                     (grouped-by-p (data:group-by p-elts-dao
                                                                                  :kv-pair #'(lambda (x)
                                                                                               (list (slot-value x 'dao:paragraph) x)))))
                                                 (setf (api-dtos:title section) section-title)
                                                 (setf (api-dtos:paragraphs section)
                                                       (hm:reduce #'(lambda (paragraphs k p-elts-dao2)
                                                                      (cons (let ((paragraph (make-instance 'api-dtos:paragraph-dto)))
                                                                              (setf (api-dtos:elements paragraph)
                                                                                    (mapcar (clos-mapping:make-mapper
                                                                                                dao:paragraph-element
                                                                                                api-dtos:paragraph-element-dto)
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
                       (cv-dto (funcall (clos-mapping:make-mapper
                                            dao:cv
                                            api-dtos:cv-dto
                                          (clos-mapping:with-computed-slot 'api-dtos:work-experiences work-experience-dtos)
                                          (clos-mapping:with-computed-slot 'api-dtos:readings reading-dtos)
                                          (clos-mapping:with-computed-slot 'api-dtos:sections section-dtos))
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
