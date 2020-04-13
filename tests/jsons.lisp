(defpackage be-it/tests/jsons
  (:use :cl
        :be-it
        :rove))
(in-package :be-it/tests/jsons)

(defclass acar ()
  ((size :type integer)
   (color :type color)
   (wheels :type wheel[])))

(defclass color ()
  (r g b))

(defclass wheel ()
  ((radius :type integer)))

(defun type-comp? (json-string)
  "Decode JSON-STRING and checks type compatibility with 'ACAR class."
  (jsons:type-compatible-p
   (json:decode-json-from-string json-string) 'acar))

(defparameter json-ok "{\"size\":12,\"wheels\":[{\"radius\":10}],\"color\":{\"r\":12,\"g\":233,\"b\":2}}")

(deftest test-type-compatible-p
  (testing "all compatible"
    (ok (type-comp? json-ok)))
  (testing "depth-1 field doesn't match slot name"
    (ok (not (type-comp? "{\"siz\":12,\"wheels\":[{\"radius\":10}]},\"color\":{\"r\":12,\"g\":233,\"b\":2}"))))
  (testing "depth-2 field doesn't match slot name"
    (ok (not (type-comp? "{\"size\":12,\"wheels\":[{\"radiu\":10}]},\"color\":{\"r\":12,\"g\":233,\"b\":2}"))))
  (testing "depth-2 field should have been an array"
    (ok (not (type-comp? "{\"size\":12,\"wheels\":{\"radius\":10},\"color\":{\"r\":12,\"g\":233,\"b\":2}}")))))
