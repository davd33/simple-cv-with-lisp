(in-package #:pipe)

(defstruct delay (value nil) (fn nil))

(defmacro delay (&body body)
  "A computation that can be executed later by FORCE."
  `(make-delay :fn #'(lambda () ,@body)))

(defun force (x)
  "Find the value of x, by computing if it is a delay."
  (if (not (delay-p x))
      x
      (progn
        (when (delay-fn x)
          (setf (delay-value x)
                (funcall (delay-fn x)))
          (setf (delay-fn x) nil))
        (delay-value x))))

(defmacro make-pipe (head tail)
  "Create a pipe by evaluating head and delaying tail."
  `(cons ,head #'(lambda () ,tail)))

(defconstant empty-pipe nil)

(defun head (pipe) (first pipe))
(defun tail (pipe)
  "Return tail of pipe or list, and destructively update
the tail if it is a function."
  (if (functionp (rest pipe))
      (setf (rest pipe) (funcall (rest pipe)))
      (rest pipe)))

(defun pipe-elt (pipe i)
  "The i-th element of a pipe, 0-based."
  (if (= i 0)
      (head pipe)
      (pipe-elt (tail pipe) (- i 1))))

(defun integers (&optional (start 0) end)
  "A pipe of integers from START to END.
If END is nil, this is an infinite pipe."
  (if (or (null end) (<= start end))
      (make-pipe start (integers (+ start 1) end))
      nil))
