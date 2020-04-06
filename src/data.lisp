(in-package #:data)

(defun group-by (sequence &key (kv-pair #'identity))
  "Group all elements of sequence, using kv-pair in order to get
  the list (key value) of an element."
  (reduce (lambda (grouped elt)
            (destructuring-bind (key val) (funcall kv-pair elt)
              (setf (gethash key grouped) (append (gethash key grouped)
                                                  (list val)))
              grouped))
          sequence
          :initial-value (make-hash-table :test 'equal)))
