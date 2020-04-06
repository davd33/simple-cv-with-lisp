(in-package #:hm)

;; Hash table utils
(defun hm-print-elt (k v &key (stream t))
  "Prints one element with K key and V value."
  (format stream "~&~a -> ~a" k v))

(defmacro hm-put (hash-table key value)
  "Adds a key/value pair in hash-table."
  `(setf (gethash ,key ,hash-table) ,value))

(defmacro hm-get (hash-table key)
  "Get value in hash-table for key."
  `(gethash ,key ,hash-table))
