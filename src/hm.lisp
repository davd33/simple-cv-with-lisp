(in-package #:hm)

;; Hash table utils

(defun print-elt (k v &key (stream t))
  "Prints one element with K key and V value."
  (format stream "~&~a -> ~a" k v))

(defmacro put (hash-table key value)
  "Adds a key/value pair in hash-table."
  `(setf (gethash ,key ,hash-table) ,value))

(defmacro get (hash-table key)
  "Get value in hash-table for key."
  `(gethash ,key ,hash-table))

(defun reduce (fn hashmap initial-value)
  "Do a reduce on a hashmap providing key and value in FN.

FN should take the following arguments:
 - ACCUMULATOR: the built result
 - KEY: the current key
 - VALUE: the current value

The initial value must be provided."
  (let ((result initial-value))
    (maphash #'(lambda (k v)
                 (setf result (funcall fn result k v)))
             hashmap)
    result))
