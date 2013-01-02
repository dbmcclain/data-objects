;; persistent-metaclass.lisp
;; --------------------------------------------------------------------------------------
;; Persistent object metaclass.
;;
;; Copyright (C) 2008 by SpectroDynamics, LLC. All rights reserved.
;;
;; DM/SD  08/08
;; --------------------------------------------------------------------------------------

;; --------------------------------------------------------------------------------------
(in-package :persistent-object)
;; --------------------------------------------------------------------------------------

;; metaclass of objects that might contain persistent slots

(defclass persistent-metaclass (clos:standard-class)
  ())

;; mixin metaclass for persistent slots and methods to make them
;; appear persistent

(defclass persistent-slot-definition (clos:standard-slot-definition)
  ((function :initarg  :function
             :accessor persistent-slot-definition-function)))

(defmethod clos:slot-definition-allocation ((slotd persistent-slot-definition))
  :persistent)


(defmethod (setf clos:slot-definition-allocation)
           (allocation (slotd persistent-slot-definition))
  (unless (eq allocation :persistent)
    (error "Cannot change the allocation of a ~S"
           'persistent-direct-slot-definition))
  allocation)

;; class of direct persistent slots and methods to construct them
;; when appropriate

(defclass persistent-direct-slot-definition (clos:standard-direct-slot-definition
                                             persistent-slot-definition)
  ())

;; Called when the class is being made, to choose the metaclass of
;; a given direct slot. It should return the class of slot definition required.

(defmethod clos:direct-slot-definition-class
           ((class persistent-metaclass) &rest initargs)
  ;; use persistent-direct-slot-definition if appropriate
  (if (eq (getf initargs :allocation) :persistent)
      (find-class 'persistent-direct-slot-definition)
    (call-next-method)))

;; Called when the defclass is expanded, to process a slot option.
;; It should return the new list of slot options, based on
;; already-processed-options

(defmethod clos:process-a-slot-option
           ((class persistent-metaclass) option value
            already-processed-options slot)
           ;; Handle the :function option by adding it to the
           ;; list of processed options.
           (if (eq option :function)
               (list* :function value already-processed-options)
             (call-next-method)))

;; Class of effective persistent slots and methods to construct
;; them when appropriate

(defclass persistent-effective-slot-definition
          (clos:standard-effective-slot-definition
           persistent-slot-definition)
  ())

;; Called when the class is being finalized, to choose the
;; metaclass of a given effective slot. It should return the
;; class of slot definition required.

(defmethod clos:effective-slot-definition-class
           ((class persistent-metaclass) &rest initargs)
  ;; Use persistent-effective-slot-definition if appropriate
  (if (eq :persistent (getf initargs :allocation))
      (find-class 'persistent-effective-slot-definition)
    (call-next-method)))

(defmethod clos:compute-effective-slot-definition
           ((class persistent-metaclass)
            name
            direct-slot-definitions)
  ;; Copy the function into the effective slot definition
  ;; if appropriate
  (let ((effective-slotd (call-next-method)))
    (dolist (slotd direct-slot-definitions)
      (when (typep slotd 'persistent-slot-definition)
        (setf (persistent-slot-definition-function effective-slotd)
              (persistent-slot-definition-function slotd))
        (return)))
    effective-slotd))

;; underlying access methods for invoking
;; persistent-slot-definition-function.

(defmethod clos:slot-value-using-class
           ((class persistent-metaclass) object slot-name)
  (let ((slotd (find slot-name (clos:class-slots class)
                     :key 'clos:slot-definition-name)))
    (if (typep slotd 'persistent-slot-definition)
        (funcall (persistent-slot-definition-function slotd)
                 :get object)
      (call-next-method))))

            
(defmethod (setf clos:slot-value-using-class)
           (value (class persistent-metaclass) object slot-name)
  (format t "~% setf slot : ~A" slot-name)
  (let ((slotd (find slot-name (clos:class-slots class)
                     :key 'clos:slot-definition-name)))
    (if (typep slotd 'persistent-slot-definition)
        (funcall (persistent-slot-definition-function slotd)
                 :set object value)
      (call-next-method))))

(defmethod clos:slot-boundp-using-class
           ((class persistent-metaclass) object slot-name)
  (let ((slotd (find slot-name (clos:class-slots class)
                     :key 'clos:slot-definition-name)))
    (if (typep slotd 'persistent-slot-definition)
        (funcall (persistent-slot-definition-function slotd)
                 :is-set object)
      (call-next-method))))

  
(defmethod clos:slot-makunbound-using-class
           ((class persistent-metaclass) object slot-name)
  (let ((slotd (find slot-name (clos:class-slots class)
                     :key 'clos:slot-definition-name)))
    (if (typep slotd 'persistent-slot-definition)
        (funcall (persistent-slot-definition-function slotd)
                 :unset object)
      (call-next-method))))

(defmethod clos:slot-exists-p-using-class
           ((class persistent-metaclass) object slot-name)
  (or (call-next-method)
      (and (find slot-name (clos:class-slots class)
                 :key 'clos:slot-definition-name)
           t)))

;; -------------------------------------------------------------------

#|
;; Example persistent slot which depends on a real slot.
;; Compile this separately after the persistent-metaclass etc.

(defclass a-base-persistent-class ()
  ()
  (:metaclass persistent-metaclass)
  (:optimize-slot-access nil))

(defclass a-persistent-class (a-base-persistent-class)
  ((real-slot  :initarg :real-slot :accessor real-slot :initform -1)
   (persistent-slot :accessor   persistent-slot
                    :initarg    :persistent-slot
                    :allocation :persistent
                    :function   'a-persistent-class-persistent-slot-function))
  (:metaclass persistent-metaclass)
  (:optimize-slot-access nil)
  )

(defun a-persistent-class-persistent-slot-function
       (key object &optional value)
  (ecase key
    (:get (let ((real-slot (real-slot object)))
            (if (<= real-slot 100)
                (/ real-slot 100.0)
              (slot-unbound (class-of object)
                            object
                            'persistent-slot))))
    (:set (setf (real-slot object) (* value 100))
     value)

    (:is-set (let ((real-slot (real-slot object)))
               (<= real-slot 100)))

    (:unset (setf (real-slot object) -1))))


(setf object (make-instance 'a-persistent-class))
(setf (persistent-slot object) 0.75)
(persistent-slot object)
(real-slot object)
(setf (real-slot object) 42)
(persistent-slot object)
|#


  
