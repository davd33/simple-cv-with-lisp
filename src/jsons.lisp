(in-package #:jsons)

(defun get-in (json &rest fields)
  (loop for f in fields
     for res = (cdr (assoc f json)) then (cdr (assoc f res))
     finally (return res)))

(defun type-compatible-p (json class-symbol)
  "Return T if JSON is compatible with CLASS."
  (labels ((contains-slot-p (json slot-name)
             "Return true if JSON contains a field SLOT-NAME."
             ;; A JSON could be:
             ;;  - a number: 12
             ;;  - a string: "hello world"
             ;;  - an object: ((:FIELD1 . 12) (:FIELD2 . 21))
             ;;  - a list of objects: (((:FIELD1 . 12) (:FIELD2 . 21))
             ;;                        ((:FIELD1 . 12) (:FIELD2 . 21)))
             ;;  - a list of values: (12 21)
             ;;  - a mix: (12 ((:FIELD1 . 12) (:FIELD2 . 21)) 21)
             (position (string slot-name) json
                       :key #'first :test #'string=)))
    (loop
       for (slot type value) in
         (mapcar #'(lambda (elt) (let ((name (closer-mop:slot-definition-name elt))
                                       (type (closer-mop:slot-definition-type elt)))
                                   (list name type
                                         (handler-case
                                             (get-in json (intern (string name) "KEYWORD"))
                                           (type-error ()
                                             (return-from type-compatible-p nil))))))
                 (mop:class-slots class-symbol))
       do
         (when (not (and (contains-slot-p json slot)
                         (if (listp value)
                             (if (str:contains? "[]" (string type))
                                 (loop
                                    for elt in value
                                    do (when (not (type-compatible-p elt
                                                   (intern
                                                    (car (str:split "[" (string type))))))
                                         (return nil))
                                    finally (return t))
                                 (type-compatible-p value type))
                             t)))
           (return nil))
       finally (return t))))

(defun add-value (value json &rest fields)
  (let ((merged-alist (apply #'alists:merge-acons value json fields)))
    (loop for k in (reduce #'(lambda (a c)
                               (adjoin (car c) a))
                           (reverse merged-alist)
                           :initial-value (list))
       collect (assoc k merged-alist))))
