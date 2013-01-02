;; biqueue.lisp -- Single reader queue with fore- and aft-queues
;; DM/RAL  02/09
;; ---------------------------------------------------------------

;; ----------------------------------------------------------------------
(in-package :com.ral.biqueue)
;; ----------------------------------------------------------------------

(defclass biqueue ()
  ((fore  :accessor biqueue-fore :initform (single-reader-mailbox:create))
   (aft   :accessor biqueue-aft  :initform (single-reader-mailbox:create))
   ))

(defun enqueue-fore (msg queue)
  (single-reader-mailbox:send msg (biqueue-fore queue)))

(defun enqueue-aft (msg queue)
  (single-reader-mailbox:send msg (biqueue-aft queue)))

(defun dequeue (queue)
  (let ((fore-q (biqueue-fore queue))
        (aft-q  (biqueue-aft  queue)))
    (labels ((peek ()
               (or (single-reader-mailbox:peek fore-q)
                   (single-reader-mailbox:peek aft-q)) )

             (grab ()
               (or (single-reader-mailbox:receive fore-q 0 nil nil)
                   (single-reader-mailbox:receive aft-q  0 nil nil))) )
      (cond ((peek) (grab))
            (t      (mpcompat:process-wait "Waiting for mail" #'peek)
                    (or (grab)
                        (dequeue queue)))
            ))))

