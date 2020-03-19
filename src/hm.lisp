(in-package #:hm)

;; Hash table utils
(defmacro hm-put (hash-table key value)
  "Adds a key/value pair in hash-table."
  `(setf (gethash ,key ,hash-table) ,value))

(defmacro hm-get (hash-table key)
  "Get value in hash-table for key."
  `(gethash ,key ,hash-table))
