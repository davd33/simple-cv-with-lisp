(in-package #:jsons)

(defun get-in (json &rest fields)
  (loop for f in fields
     for res = (cdr (assoc f json)) then (cdr (assoc f res))
     finally (return res)))

(defun add-value (value json &rest fields)
  (let ((merged-alist (apply #'alists:merge-acons value json fields)))
    (loop for k in (reduce #'(lambda (a c)
                               (adjoin (car c) a))
                           (reverse merged-alist)
                           :initial-value (list))
       collect (assoc k merged-alist))))
