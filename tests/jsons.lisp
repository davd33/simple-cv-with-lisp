(defpackage be-it/tests/jsons
  (:use :cl
        :be-it
        :rove))
(in-package :be-it/tests/jsons)

(defclass acar ()
  ((size :type integer)
   (wheels :type wheel)))

(defclass wheel ()
  ((radius :type integer)))

(progn
  (format t "~&FINALIZE CLASSES...~%")
  (make-instance 'acar)
  (make-instance 'wheel))

(defun type-comp? (json-string)
  "Decode JSON-STRING and checks type compatibility with 'ACAR class."
  (jsons:type-compatible-p
   (json:decode-json-from-string json-string) 'acar))

(deftest test-type-compatible-p
  (testing "all compatible"
    (ok (type-comp? "{\"size\":12,\"wheels\":{\"radius\":10}}")))
  (testing "depth-1 field doesn't match slot name"
    (ok (not (type-comp? "{\"siz\":12,\"wheels\":{\"radius\":10}}"))))
  (testing "depth-2 field doesn't match slot name"
    (ok (not (type-comp? "{\"size\":12,\"wheels\":{\"radiu\":10}}")))))
