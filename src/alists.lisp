(in-package :alists)

(defun aconses (alist &rest fields)
  "Adds several key/value fields to alists and return the new version."
  (loop for (k v) in fields
     for res = (acons k v alist) then (acons k v res)
     finally (return res)))
