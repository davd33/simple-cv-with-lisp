(in-package #:services)

(defun get-cv (cv-title)
  "Retrieves CV in DB by title."
  (handler-case (let* (;; RETRIEVE ALL DATA
                       (cv-dao (dao:retrieve-cv cv-title))
                       (cv-id (slot-value cv-dao 'id))
                       (readings (dao:retrieve-readings cv-id))
                       (work-experiences (dao:retrieve-work-experiences cv-id))
                       (paragraph-elements (dao:retrieve-paragraph-elements cv-id))
                       ;; CREATE NEEDED MAPPERS
                       (reading-dao->dto (clos-mapping:make-mapper
                                             dao:reading
                                             api-dtos:reading-dto))
                       (work-experience-dao->dto (clos-mapping:make-mapper
                                                     dao:work-experience
                                                     api-dtos:work-experience-dto))
                       (reading-dao->dto (clos-mapping:make-mapper
                                             dao:reading
                                             api-dtos:reading-dto))
                       (cv-dao->dto (clos-mapping:make-mapper
                                        dao:cv
                                        api-dtos:cv-dto))
                       ;; BUILD CV DTO
                       (cv-dto (funcall cv-dao->dto)))
                  "CV DTO has been retrieved!")
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
