(in-package :alists)

(defun deep-acons (value alist &rest fields)
  "Like acons, but actually arbitrary acons of acons."
  (let ((rfields (reverse fields)))
    (loop for i in rfields
       for res = (acons i value nil) then (if (not (equalp i (car (last rfields))))
                                              (acons i res nil)
                                              res)
       finally (return (acons i res alist)))))

(defun aconses (alist &rest fields)
  "Adds several key/value fields to alists and return the new version."
  (loop for (k v) in fields
     for res = (if (listp k)
                   (apply #'deep-acons v alist k)
                   (acons k v alist)) then (if (listp k)
                                               (apply #'deep-acons v res k)
                                               (acons k v res))
     finally (return res)))
