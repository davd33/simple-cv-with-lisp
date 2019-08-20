(defpackage be-it/tests/main
  (:use :cl
        :be-it
        :rove))
(in-package :be-it/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :be-it)' in your Lisp.

(deftest test-target-1
  (testing "should (= 1 1) to be true"
    (ok (= 1 1))))
