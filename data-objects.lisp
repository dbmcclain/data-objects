;; data-objects.lisp -- Intelligent Queues etc.
;;
;; DM/HMSC  11/97
;; -------------------------------------------------------------

(in-package "DATA-OBJECTS")

(defgeneric get-item     (obj      &rest lock-args))
(defgeneric put-item     (obj item &rest lock-args))
(defgeneric data-available-p (obj))

(defclass basic-data-object ()
  ())

;; ---------------------------------------------------------
;; Basic FIFO Queues
;;

(defclass basic-FIFO-queue (basic-data-object)
  ((data :accessor queue-data
	 :initform nil)))

(defmethod get-item ((queue basic-FIFO-queue) &rest lock-args)
  (declare (ignore lock-args))
  (pop (queue-data queue)))

(defmethod put-item ((queue basic-FIFO-queue) item &rest lock-args)
  (declare (ignore lock-args))
  (um:conc1f (queue-data queue) item))

(defmethod data-available-p ((queue basic-FIFO-queue))
  (queue-data queue))

;; ---------------------------------------------------------
;; Basic LIFO Stacks
;;

(defclass basic-LIFO-stack (basic-FIFO-queue)
  ((data :accessor stack-data
	 :initform nil)))

(defmethod put-item ((stack basic-LIFO-stack) item &rest lock-args)
  (declare (ignore lock-args))
  (push item (stack-data stack)))


;; ---------------------------------------------------------
;; Bugfix versions for HQN 4.1x
#| -- Fix for 4.1x from Brigette Bovy.
   This processes messages from a hidden thread window.
   Apparently, Harlequin implements threads by means of the old Win/3.1
   hack of using hidden windows...
   Hacked to support timeouts by DM/MCFA 5/10/00.
|#
;;
#|
(defun my-process-wait (reason wait-fn &rest wait-args)
  (flet ((wf ()
             (apply wait-fn wait-args)))
    (win32::process-messages nil
                             :exit-function #'wf
                             :wait-function #'wf
                             :wait-reason   reason)))

(defun my-process-wait-with-timeout (reason timeout
                                            &optional wait-fn
                                            &rest wait-args)
  (let ((timer (mp:make-timer 'true))
        (timed-out-p nil))
    (flet ((wf ()
               (or (setf timed-out-p (mp:timer-expired-p timer))
                   (apply wait-fn wait-args))))
      (mp:schedule-timer-relative timer timeout)
      (my-process-wait reason #'wf)
      (mp:unschedule-timer timer)
      (not timed-out-p))))

(defun my-mailbox-read (mbox &optional wait-reason)
  (my-process-wait wait-reason (complement 'mp:mailbox-empty-p) mbox))

(defun my-process-lock (lock &optional whostate timeout)
  (or (mp:claim-lock lock)
      (if timeout
          (my-process-wait-with-timeout
           whostate timeout 'mp:claim-lock lock)
        (my-process-wait
         whostate 'mp:claim-lock lock))))
|#
;; --------------------------------------------------------
;; Is it a problem with the dobj:my-... routines?
#|
(defun my-mailbox-read (&rest args)
  (apply 'mpcompat:mailbox-read args))

(defun my-process-lock (&rest args)
  (apply 'mpcompat:process-lock args))

(defun my-process-wait (&rest args)
  (apply 'mpcompat:process-wait args))

(defun my-process-wait-with-timeout (&rest args)
  (apply 'mpcompat:process-wait-with-timeout args))

;; --------------------------------------------------------
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro my-with-lock ((lock &rest lock-args) &body body)
    (let ((lok (gensym)))
      `(let ((,lok ,lock))
	(apply 'my-process-lock ,lok ,@lock-args)
	(unwind-protect
	     (progn
	       ,@body)
	  (mpcompat:process-unlock ,lok))))))
|#
;; ---------------------------------------------------------
;; Lock mixin class for MP safe data access
;;

(defclass mp-shared-mixin ()
  ((lock :accessor mp-shared-lock
	 :initform (mpcompat:make-lock :name "mp-shared-lock"))))

(defmethod lock ((v mp-shared-mixin) &optional whostate timeout)
  (mpcompat:process-lock (mp-shared-lock v) whostate timeout))

(defmethod unlock ((v mp-shared-mixin))
  (mpcompat:process-unlock (mp-shared-lock v)))


(defmethod with-locked-access (obj fn &optional whostate timeout)
  (declare (ignore obj whostate timeout))
  (values (funcall fn) t))

(defmethod with-locked-access ((obj mp-shared-mixin) fn
                               &optional whostate timeout)
  (mpcompat:with-spinlock ((mp-shared-lock obj) whostate timeout)
    (values (funcall fn) t)))

;; --------------------------------------------------------
;; MP Safe Data Objects

(defclass mp-shared-data-object
          (mp-shared-mixin basic-data-object)
  ())

(defmethod data-available-p ((obj mp-shared-data-object))
  ;; return nil if either can't lock the object,
  ;; or else the object has no data available.
  ;; This is a polled call - no blocking.
  (with-locked-access obj
    #'(lambda ()
        (call-next-method))
    nil 0))

(defmethod get-item ((obj mp-shared-data-object) &rest lockargs)
  ;; return (values data t) if successful, else nil on timeout
  (let ((whostate (first  lockargs))
        (timeout  (second lockargs)))
    (prog ()
      again
      (multiple-value-bind (val locked)
          (with-locked-access obj
                              #'(lambda ()
                                  (if (data-available-p obj)
                                      (list (call-next-method) t)))
                              whostate timeout)
        (if locked
            (multiple-value-bind (data avail) (apply 'values val)
              (if avail
                  (return (values data t))
                (if (mpcompat:process-wait-with-timeout whostate timeout
                                                        'data-available-p obj)
                    (go again)))))
        ))))

(defmethod put-item ((obj mp-shared-data-object) item &rest lockargs)
  (declare (ignore item))
  (let ((whostate (first  lockargs))
        (timeout  (second lockargs)))
    (with-locked-access obj
      #'(lambda ()
          (call-next-method))
      whostate timeout)))

;; --------------------------------------------------------
(defclass condition-variable ()
  ((waiting-list :accessor cx-waiting-list :initform nil)))

(defclass mutex (mp-shared-mixin)
  ())

(defun make-mutex ()
  (make-instance 'mutex))

(defun make-condition-variable ()
  (make-instance 'condition-variable))

(defmethod cx-empty-p ((cx condition-variable))
  (null (cx-waiting-list cx)))

(defmethod cx-wait ((cx condition-variable) (m mutex))
  (let ((mb (mpcompat:make-mailbox :size 1 #|:lock-name "cx-lock"|#)))
    (um:conc1f (cx-waiting-list cx) mb)
    (unlock m)
    (let ((ans (mpcompat:mailbox-read mb "waiting for condition signal")))
      (lock m)
      ans)))

(defmethod cx-signal ((cx condition-variable) &optional val)
  (let ((p (pop (cx-waiting-list cx))))
    (if p
        (mpcompat:mailbox-send p val))
    ))

(defmethod cx-broadcast ((cx condition-variable) &optional val)
  (do ((p (pop (cx-waiting-list cx)) (pop (cx-waiting-list cx))))
      ((null p))
    (mpcompat:mailbox-send p val)))

(defmethod with-condition-var ((cx condition-variable) (m mutex) &key test oper)
  (with-locked-access m
                      #'(lambda ()
                          (um:until (funcall test)
                            (cx-wait cx m))
                          (funcall oper))))


;; --------------------------------------------------------
;; MP Safe FIFO Queues

(defclass mp-shared-FIFO-queue (mp-shared-data-object basic-FIFO-queue)
  ())

;; --------------------------------------------------------
;; MP Safe LIFO Stacks
;;

(defclass mp-shared-LIFO-stack (mp-shared-data-object basic-LIFO-stack)
  ())

;; -- end of data-objects.lisp -- ;;
