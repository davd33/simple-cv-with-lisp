(in-package #:clos-mapping)

(defmacro make-mapper (from-class to-class &body body)
  "Creates a function that, given an input object of type 'from-class',
will make a new instance of type 'to-class' setting all name-identique
slots' values equal.

In the body, you can setf slots for which the name wasn't equivalent in input and
output classes.
Available functions in the body:
  WITH-RENAMED-SLOT (old-name new-name)
    Copies the value of the 'old-name' slot in the from object to the 'new-name' slot of
    the destination object.
  WITH-COMPUTED-SLOT (slot-name value)
    Sets value of the 'slot-name' slot of the destination object to 'value'."
  (restart-case (let* ((slot-names-fn (compose #'(lambda (slots)
                                                   (mapcar #'(lambda (elt)
                                                               (closer-mop:slot-definition-name elt))
                                                           slots))
                                               #'closer-mop:class-slots
                                               #'find-class))
                       (from-slots (funcall slot-names-fn from-class))
                       (to-slots (funcall slot-names-fn to-class)))
                  `(lambda (from-obj)
                     (let ((to-obj (make-instance ',to-class)))
                       (loop for to-s in ',to-slots
                          do (when-let* ((from-s (find to-s ',from-slots
                                                       :test #'(lambda (elt1 elt2)
                                                                 (string= (string elt1)
                                                                          (string elt2)))))
                                         (from-value (handler-case (slot-value from-obj from-s)
                                                       (unbound-slot (e) nil))))
                               (setf (slot-value to-obj to-s) from-value)))
                       (labels ((with-renamed-slot (old-name new-name)
                                  (setf (slot-value to-obj new-name)
                                        (slot-value from-obj old-name)))
                                (with-computed-slot (slot-name value)
                                  (setf (slot-value to-obj slot-name)
                                        value)))
                         ,@body)
                       to-obj)))
    (finalized-class ()
      (macroexpand `(make-mapper ,from-class ,to-class)))))
