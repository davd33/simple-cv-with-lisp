(in-package #:jsons)

(defun get-in (json &rest fields)
  (loop for f in fields
     for res = (cdr (assoc f json)) then (cdr (assoc f res))
     finally (return res)))

(defun type-compatible-p (json class-symbol)
  "Returns T if JSON is compatible with CLASS."
  (labels ((contains-slot-p (json slot-name)
             "Return true if JSON doesn't contain a field SLOT-NAME."
             (position (string slot-name) json
                       :key #'first :test #'string=)))
    (loop
       for (slot type value) in (mapcar #'(lambda (elt) (let ((name (closer-mop:slot-definition-name elt))
                                                              (type (closer-mop:slot-definition-type elt)))
                                                          (list name type
                                                                (get-in json (intern (string name) "KEYWORD")))))
                                        (mop:class-slots class-symbol))
       do
         (format t "~&TYPE = ~A~%" type)
         (format t "~&SLOT = ~A~%" slot)
         (format t "~&VALUE = ~A~%" value)
         (when (not (and (contains-slot-p json slot)
                       (if (listp value)
                           (type-compatible-p value type)
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
