;; priority-queue.lisp -- Priority Queue based on Binary Trees
;; DM/RAL  02/09
;; --------------------------------------------------------------

(in-package :com.ral.priority-queue)
            
;; -------------------------------------------------------------------------------------------

(defclass priority-queue ()
  ((lock   :accessor priority-queue-lock               :initform (mpcompat:make-lock))
   (condv  :accessor priority-queue-condition-variable :initform (mpcompat:make-condition-variable))
   (count  :accessor priority-queue-count              :initform 0)
   (top    :accessor priority-queue-top                :initform nil) ;; a tree root
   ))

(defun add-item (priq key item)
  (with-accessors ((lock  priority-queue-lock)
                   (top   priority-queue-top)
                   (count priority-queue-count)
                   (condv priority-queue-condition-variable)) priq
    (mpcompat:with-spinlock (lock)
      (setf top (maps:add key item top))
      (incf count)
      (mpcompat:condition-variable-signal condv)) ))

(defun remove-item (priq)
  (with-accessors ((top   priority-queue-top)
                   (count priority-queue-count)
                   (lock  priority-queue-lock)
                   (condv priority-queue-condition-variable)) priq
    (mpcompat:with-spinlock (lock)
      (when (maps:is-empty top)
        (mpcompat:condition-variable-wait condv lock
                                          :wait-reason "Waiting for priority mail"))
      (let ((val (maps::map-cell-val (sets:min-elt top))))
        (setf top (sets:remove-min-elt top))
        (decf count)
        val) )))

;; ------------------------------------------------------------

#|
(progn
  (defun child-nodes-for-viewing (node)
    (when node
      (let* ((l (sets-internal::node-left node))
             (r (sets-internal::node-right node))
             (v (and l (list l))))
        (if r
          (cons r v)
          v))))
  
  (defun node-value-string (node)
    (if node
      (format nil "~A" (maps::map-cell-key (sets-internal::node-val node)))
      ))
  
  (defun view-tree (tree)
    (CAPI:CONTAIN
     (MAKE-INSTANCE 'CAPI:GRAPH-PANE
                    :ROOTS (list tree)
                    :CHILDREN-FUNCTION
                    'child-nodes-for-viewing
                    :PRINT-FUNCTION
                    'node-value-string)))
  
  (defun view-queue (q)
    (view-tree (priority-queue-top q))) )

|#
