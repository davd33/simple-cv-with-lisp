(in-package #:jsons)

(defun get-in (json &rest fields)
  (first (last (loop for f in fields
                  for res = (cdr (assoc f json)) then (cdr (assoc f res))
                  collect res))))
