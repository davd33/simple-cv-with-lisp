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

(defun foreach (pipe &key count do (result pipe))
  "Go through all (or COUNT) elements of PIPE,
possibly applying the DO function (Try PRINT.)
Take care of providing a COUNT parameter (integer) for infinite pipes."
  ;; Returns RESULT, which defaults to the pipe itself
  (labels ((next-count (c)
             (cond ((> c 0) (1- c))
                   ((< c 0) (1+ c))
                   (t c))))
    (if (or (eq pipe empty-pipe)
            (eql count 0))
        result
        (progn
          (unless (null do) (funcall do (head pipe)))
          (foreach (tail pipe) :count (if count (next-count count))
                   :do do :result result)))))

;;; TODO
(defun filter (pred pipe &key count)
  "Keep only items in PIPE satisfying PRED.
Filter all (or COUNT) items."
  (if (and count (funcall pred (head pipe)))
      (make-pipe (head pipe)
                 (filter pred (tail pipe)))
      (filter pred (tail pipe))))
