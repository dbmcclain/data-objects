;; multiple-reader-mailbox.lisp
;; --------------------------------------------------------------------------------------
;; Multiple-reader-mailboxes = queues with a lock
;; These mailboxes are intended for use by multiple reader threads. (DM/SD 02/09)
;;
;; Copyright (C) 2008 by SpectroDynamics, LLC. All rights reserved.
;;
;; DM/SD  08/08
;; --------------------------------------------------------------------------------------

;; --------------------------------------------------------------------------------------
(in-package :multiple-reader-mailbox)
;; --------------------------------------------------------------------------------------

(defstruct (mailbox
            (:include queue:queue))
  (lock  (mpcompat:make-lock)               :read-only t)
  (condv (mpcompat:make-condition-variable) :read-only t))

(defun create ()
  (make-mailbox))

(defun send (msg mbox)
  (with-accessors ((lock  mailbox-lock)
                   (condv mailbox-condv)) mbox
    (mpcompat:with-spinlock (lock)
      (queue:add msg mbox)
      (mpcompat:condition-variable-signal condv)) ))
  
(defun receive (mbox &optional timeout (timeout-error-p t) timeout-value)
  (with-accessors ((lock  mailbox-lock)
                   (condv mailbox-condv)) mbox

      (mpcompat:with-spinlock (lock)
        (if (or (not-empty mbox)
                (and (or (null timeout)
                         (plusp timeout))
                     (mpcompat:condition-variable-wait condv lock
                                                       :wait-reason "Waiting for mail"
                                                       :timeout     timeout)))
            (queue:pop mbox)
          ;; else
          (if timeout-error-p
              (error "Timed out waiting for mail")
            ;; else
            (values timeout-value nil)) )) ))


