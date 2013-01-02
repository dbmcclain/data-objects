;; stacks.lisp
;; --------------------------------------------------------------------------------------
;; Stacks using Lists and Adjustable Vectors
;;
;; Copyright (C) 2008 by SpectroDynamics, LLC. All rights reserved.
;;
;; DM/SD  08/08
;; --------------------------------------------------------------------------------------

;; -------------------------------------------------------------------
(in-package :stack-on-list)
;; -------------------------------------------------------------------

(defstruct stack
  (depth 0)
  (stk nil))

(defun create ()
  (make-stack))

(defun clear (s)
  (setf (stack-stk s) nil
        (stack-depth s) 0))

(defun copy (s)
  (make-stack
   :stk   (stack-stk s)
   :depth (stack-depth s)))

(defun push (x s)
  (cl:push x (stack-stk s))
  (incf (stack-depth s)))

(defun check-stack (s fn empty-error-p empty-value)
  (if (null (stack-stk s))
      (if empty-error-p
          (error "Stack empty")
        (values empty-value nil))
    (values (funcall fn) t)))

(defun pop (s &optional (empty-error-p t) empty-value)
  (check-stack s #'(lambda ()
                     (decf (stack-depth s))
                     (cl:pop (stack-stk s)))
               empty-error-p empty-value))

(defun top (s &optional (empty-error-p t) empty-value)
  (check-stack s #'(lambda ()
                     (first (stack-stk s)))
               empty-error-p empty-value))

(defun is-empty (s)
  (null (stack-stk s)))

(defun depth (s)
  (stack-depth s))

(defun iter (f s)
  (dolist (item (stack-stk s))
    (funcall f item)))


;; -------------------------------------------------------------------
(in-package :stack-on-vector)
;; -------------------------------------------------------------------

(defstruct stack
  (stk (make-array 0
                   :adjustable t
                   :fill-pointer t)))

(defun create ()
  (make-stack))

(defun clear (s)
  (setf (fill-pointer (stack-stk s)) 0))

(defun copy (s)
  ;; ?? questionable...
  (make-stack
   :stk (stack-stk s)))

(defun push (x s)
  (vector-push-extend x (stack-stk s)))

(defun check-stack (s fn empty-error-p empty-value)
  (if (zerop (fill-pointer (stack-stk s)))
      (if empty-error-p
          (error "Stack empty")
        (values empty-value nil))
    (values (funcall fn) t)))

(defun pop (s &optional (empty-error-p t) empty-value)
  (check-stack s #'(lambda ()
                     (vector-pop (stack-stk s)))
               empty-error-p empty-value))

(defun top (s &optional (empty-error-p t) empty-value)
  (check-stack s #'(lambda ()
                     (let ((stk (stack-stk s)))
                       (aref stk (1- (fill-pointer stk)))))
               empty-error-p empty-value))

(defun is-empty (s)
  (zerop (fill-pointer (stack-stk s))))

(defun depth (s)
  (fill-pointer (stack-stk s)))

(defun iter (f s)
  (let ((stk (stack-stk s)))
    (dotimes (ix (fill-pointer stk))
      (funcall f (aref stk ix))) ))



