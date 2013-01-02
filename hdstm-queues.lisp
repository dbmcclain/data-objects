;; hdstm-queues.lisp
;; --------------------------------------------------------------------------------------
;; Fast FIFO Queues using circular lists
;;
;; Copyright (C) 2008 by SpectroDynamics, LLC. All rights reserved.
;;
;; DM/SD  08/08
;; --------------------------------------------------------------------------------------

(defpackage :hqueue
  (:use #:common-lisp)
  (:shadow
   #:push
   #:pop
   #:length
   #:delete
   #:delete-if
   #:find
   #:find-if
   #:map
   #:every
   #:some
   #:position
   #:position-if
   #:nth
   #:count
   #:count-if
   #:reduce
   #:member
   )
  (:export
   #:queue
   #:create
   #:clear
   #:add
   #:push
   #:peek
   #:top
   #:take
   #:pop
   #:copy
   #:is-empty
   #:not-empty
   #:length
   #:map
   #:iter
   #:fold
   #:transfer
   #:contents
   #:tail
   #:delete
   #:delete-if
   #:find
   #:find-if
   #:every
   #:some
   #:list-of
   #:position
   #:position-if
   #:count
   #:count-if
   #:nth
   #:reduce
   #:member
   #:do-queue
   ))

;; -------------------------------------------------------------------
(in-package :hqueue)
;; -------------------------------------------------------------------
;; FIFO queues with in-place modification
;;
;; A queue is a reference to either nothing or some cell of a cyclic
;; list.  By convention, that cell is to be viewed as the last cell in
;; the queue.  The first cell in the queue is then found in constant
;; time: it is the next cell in the cyclic list. The queue's length is
;; also recorded, so as to make (length) a constant-time operation.

(defstruct queue
  (length 0)
  (tail   nil))

(defun create ()
  (make-queue))

(defun clear (q)
  (setf (queue-length q) 0
        (queue-tail q)   nil))

;; -----------------------------------------------------

(defun contents (q)
  ;; destructively read the queue
  ;; returning its contents as a list
  ;; the queue structure is then reset to empty
  (um:when-let (lst (shiftf (queue-tail q) nil))
    (setf (queue-length q) 0)
    (shiftf (cdr lst) nil)))

(defun set-contents! (q lst)
  ;; forcibly make lst the contents of the queue
  (setf (queue-tail q)   (last lst)
        (queue-length q) (cl:length lst))
  (when lst
    (setf (cdr (queue-tail q)) lst)))

(defun set-contents (q lst)
  ;; safely make lst the contents of the queue
  (set-contents! q (and lst (copy-list lst))))

(defsetf contents (q) (lst)
  `(set-contents ,q ,lst))

;; -----------------------------------------------------

(defun do-as-list (q fn)
  (um:when-let (tl (queue-tail q))
    (let ((hd (shiftf (cdr tl) nil)))
      (unwind-protect
          (funcall fn hd)
        (setf (cdr tl) hd))
      )))

(defmacro with-queue-as-list ((q lst-name) &body body)
  `(do-as-list ,q (lambda (,lst-name) ,@body)))

#+:LISPWORKS
(editor:setup-indent "with-queue-as-list" 1)

;; -----------------------------------------------------

(defun add (x q)
  (let ((cell (cons x nil)))
    (um:if-let (tail (queue-tail q))
        (setf (cdr cell) (cdr tail)
              (cdr tail) cell)
      ;; else
      (setf (cdr cell) cell))
    (setf (queue-tail q) cell)
    (incf (queue-length q))
    x))

(defun push (x q)
  (add x q))

(defun peek (q &optional empty-value)
  (if (queue-tail q)
      (values (cadr (queue-tail q)) t)
    (values empty-value nil)))

(defun top (q &optional empty-value)
  (peek q empty-value))

(defun tail (q)
  (queue-tail q))

(defun take (q &optional (empty-error-p t) empty-value)
  (um:if-let (tail (queue-tail q))
      (let ((head (cdr tail)))
        (if (eq head tail)
            (setf (queue-tail q) nil)
          (setf (cdr tail) (cdr head)))
        (decf (queue-length q))
        (values (car head) t))
    ;; else
    (if empty-error-p
        (error "Queue empty")
      (values empty-value nil)) ))

(defun pop (q &optional (empty-error-p t) empty-value)
  (take q empty-error-p empty-value))

(defun copy (q)
  (let ((qcopy (copy-queue q)))
    (with-queue-as-list (q qlst)
      (setf (contents qcopy) qlst)
      qcopy)))

(defun not-empty (q)
  (queue-tail q))

(defun is-empty (q)
  (null (queue-tail q)))

(defun length (q)
  (queue-length q))

(defun iter (f q)
  (with-queue-as-list (q qlst)
      (dolist (qitem qlst)
        (funcall f qitem))))

(defun map (f q)
  (with-queue-as-list (q qlst)
      (mapcar f qlst)))

(defun fold (f accu q)
  (with-queue-as-list (q qlst)
    (cl:reduce f qlst :initial-value accu)))

(defun transfer (q1 q2)
  (set-contents! q2 (nconc (contents q1)
                           (contents q2))))

(defun delete (item q)
  (set-contents! q (cl:delete item (contents q))))

(defun delete-if (pred q)
  (set-contents! q (cl:delete-if pred (contents q))))

(defun find (item q &rest args)
  (with-queue-as-list (q lst)
    (apply 'cl:find item lst args)))

(defun find-if (fn q &rest args)
  (with-queue-as-list (q lst)
    (apply 'cl:find-if fn lst args)))

(defun every (fn q)
  (with-queue-as-list (q lst)
    (cl:every fn lst)))

(defun some (fn q)
  (with-queue-as-list (q lst)
    (cl:some fn lst)))

(defun list-of (q)
  (with-queue-as-list (q lst)
    (copy-list lst)))

(defun count (item q &rest args)
  (with-queue-as-list (q lst)
    (apply 'cl:count item lst args)))

(defun count-if (pred q &rest args)
  (with-queue-as-list (q lst)
    (apply 'cl:count-if pred lst args)))

(defun reduce (fn q &rest args)
  (with-queue-as-list (q lst)
    (apply 'cl:reduce fn lst args)))

(defun nth (n q)
  (with-queue-as-list (q lst)
    (cl:nth n lst)))

(defun position (item q &rest args)
  (with-queue-as-list (q lst)
    (apply 'cl:position item lst args)))

(defun position-if (pred q &rest args)
  (with-queue-as-list (q lst)
    (apply 'cl:position-if pred lst args)))

(defun member (item q &rest args)
  (with-queue-as-list (q lst)
    (apply 'cl:member item lst args)))

(um:defmacro! do-queue ((item q &optional ans) &body body)
  `(with-queue-as-list (,q ,g!lst)
     (dolist (,item ,g!lst ,ans)
       ,@body)))

#+:LISPWORKS
(editor:setup-indent "do-queue" 1)

