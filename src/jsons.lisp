(in-package #:jsons)

(defun get-in (json &rest fields)
  (loop for f in fields
     for res = (cdr (assoc f json)) then (cdr (assoc f res))
     finally (return res)))
