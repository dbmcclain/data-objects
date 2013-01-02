;; single-reader-mailbox.lisp
;; --------------------------------------------------------------------------------------
;; Single-reader-mailboxes = queues with a lock
;; These mailboxes are intended for use by a single reader thread.
;; Multiple readers would need additional locking.  (DM/SD  02/09)
;;
;; Copyright (C) 2008 by SpectroDynamics, LLC. All rights reserved.
;;
;; DM/SD  08/08
;; --------------------------------------------------------------------------------------

;; --------------------------------------------------------------------------------------
(in-package :single-reader-mailbox)
;; --------------------------------------------------------------------------------------

(defstruct (mailbox
            (:include queue:queue))
  (lock  (mpcompat:make-lock)               :read-only t)
  (condv (mpcompat:make-condition-variable) :read-only t))

;; ----------------------------------------------------------------

(defun create ()
  (make-mailbox))

;; ----------------------------------------------------------------

(defun send (msg mbox)
  (with-accessors ((lock  mailbox-lock)
                   (condv mailbox-condv)) mbox
    (mpcompat:with-spinlock (lock)
      (queue:add msg mbox)
      (mpcompat:condition-variable-signal condv)) ))

;; ----------------------------------------------------------------

(defun receive (mbox &optional timeout (timeout-error-p t) timeout-value)
  (with-accessors ((lock  mailbox-lock)
                   (condv mailbox-condv)) mbox
    (mpcompat:with-spinlock (lock)
      (if (or (not-empty mbox)
              (mpcompat:condition-variable-wait condv lock
                                                :wait-reason "Waiting for mail"
                                                :timeout timeout))
          (queue:pop mbox) ;; returns val, t/f
        ;; else
        (if timeout-error-p
            (error "Timed out waiting for mail")
          ;; else
          (values timeout-value nil) )))))

;; ----------------------------------------------------------------

#|
(defun selective-receive (mbox fn
			  &optional
			  exit-at-end
			  timeout
			  (timeout-error-p t)
			  timeout-value)
  
  (let ((lock (mailbox-lock mbox)))
    
    (labels ((get-head ()
               (mpcompat:with-spinlock (lock)
                 (cdr (queue:tail mbox))))
             
             (iter (cursor follow)
               (let ((msg (car cursor)))
                 (if (funcall fn msg)
                     (mpcompat:with-spinlock (lock)
                       (queue:remove-at mbox (or follow (queue:tail mbox)))
                       (values msg t))
                   ;; else
                   (if (eq cursor (queue:tail mbox))
                       (if exit-at-end
                           (values nil nil)
                         ;; else
                         (multiple-value-bind (ans found)
                             (wait-for-message cursor)
                           (if found
                               (iter (cdr cursor) cursor)
                             ;; else
                             (values ans nil))))
                     ;; else
                     (iter (cdr cursor) cursor))
                   )))
        
             (wait-for-message (last)
               (if (mpcompat:process-wait-with-timeout
                    "Waiting for mail" timeout
                    (lambda ()
                      (not (eq last (queue:tail mbox)))))
                   (values t t)
                 ;; else
                 (if timeout-error-p
                     (error "Timed out waiting for mail")
                   ;; else
                   (values timeout-value nil)))) )

      (if (not-empty mbox)
          (iter (get-head) nil)
        ;; else
        (multiple-value-bind (ans found)
            (wait-for-message nil)
          (if found
              (iter (get-head) nil)
            ;; else
            (values ans nil))
          ))
      )))
|#
