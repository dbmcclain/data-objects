;; reppy-channels.lisp -- Channels a'la Reppy's CML
;;
;; DM/HMSC  01/00
;; -------------------------------------------------------------

(in-package "REPPY-CHANNELS")

;; -------------------------------------------------------
;; Reppy CML Style Channels and Events with Combinators

(defun spawn (fn &rest args)
  (apply 'mpcompat:process-run-function (gensym) nil fn args))


;; -----------------------------------------------------------------------

(defclass channel ()
  ((valid   :accessor channel-valid
            :initform t)
   (readers :accessor channel-pending-readers
            :initform nil)
   (writers :accessor channel-pending-writers
            :initform nil)
   ))

(defun make-channel ()
  (make-instance 'channel))


;; -----------------------------------------------------------------------

(defclass comm-cell ()
   ;; ref cell (NIL = not yet sync'd, >= 0 sync'd)
  ((performed :accessor comm-performed :initarg :performed)
   ;; the sync mailbox
   (mbox      :accessor comm-mbox      :initarg :mbox)
   ;; the comm data value
   (data      :accessor comm-data      :initarg :data)
   ;; the comm event number for this comm
   (evnum     :accessor comm-evnum     :initarg :evnum)
   ;; a ref cell holding the number of valid channels waited on
   (nrvalid   :accessor comm-nrvalid   :initarg :nrvalid)
   ))

;; -----------------------------------------------------------------------

(defclass behaviors ()
   ;; function to poll for comm (t if data avail)
  ((poll    :accessor bev-poll    :initarg :poll)
   ;; function to suspend on channel
   (suspend :accessor bev-suspend :initarg :suspend)
   ;; function to return result of comm
   (result  :accessor bev-result  :initarg :result)
   ))

;; -----------------------------------------------------------------------

(defclass event ()
  ())

;; -----------------------------------------------------------------------

(defclass comm-event (event)
  ((behavior :accessor comm-behavior :initarg :behavior)
   ))

;; -----------------------------------------------------------------------


(defclass ref-cell ()
  ((value :accessor ref-cell-value :initarg :value)
   ;; provides a level of indirection
   ))

(defun make-ref (val)
  (make-instance 'ref-cell :value val))

(defclass bev-cell ()
  ((bev    :accessor bev-cell-bev       :initarg :bev)
   (aborts :accessor bev-cell-abort-ids :initarg :abort-ids)))

(defclass abort-cell ()
  ((id   :accessor abort-cell-id   :initarg :id)
   (fn   :accessor abort-cell-fn   :initarg :fn)
   (args :accessor abort-cell-args :initarg :args)))

(defmethod already-syncd-p  ((cell comm-cell))
  (ref-cell-value (comm-performed cell)))

;; -----------------------------------------------------------------------

(defmethod sendEvt ((ch channel) data)
  (make-instance 'comm-event
                 :behavior
    (lambda (performed mbox evnum nrvalid)
      (let ((wcomm (make-instance
                    'comm-cell
                    :performed performed
                    :mbox      mbox
                    :data      data
                    :evnum     evnum
                    :nrvalid   nrvalid)))
        (make-instance
         'behaviors
         :poll (lambda ()
                   (if (channel-valid ch)
                       (prog ()
                         again
                         (let ((rcomm (pop (channel-pending-readers ch))))
                           (if rcomm
                               (if (already-syncd-p rcomm)
                                   (go again)
                                 (block not-syncd
                                   (setf
                                    (comm-data rcomm) (comm-data wcomm)
                                    (ref-cell-value performed) evnum
                                    (ref-cell-value (comm-performed rcomm))
                                    (comm-evnum rcomm))
                                   (mpcompat:mailbox-send (comm-mbox rcomm) t)
                                   (return t))))
                           ))))
         :suspend (lambda ()
                      (if (channel-valid ch)
                          (setf (channel-pending-writers ch)
                                (um:conc1 (cleanup-queue (channel-pending-writers ch))
                                       wcomm))
                        (decf (ref-cell-value nrvalid))))
         :result (constantly nil)
         )))))

(defvar *no-data* #(:no-data))  ;; unique under eq

(defmethod recvEvt ((ch channel))
  (make-instance 'comm-event
                 :behavior
  (lambda (performed mbox evnum nrvalid)
      (let ((rcomm (make-instance
                    'comm-cell
                    :performed performed
                    :mbox      mbox
                    :data      *no-data*
                    :evnum     evnum
                    :nrvalid   nrvalid)))
        (make-instance
         'behaviors
         :poll (lambda ()
                   (if (channel-valid ch)
                       (prog ()
                         again
                         (let ((wcomm (pop (channel-pending-writers ch))))
                           (if wcomm
                               (if (already-syncd-p wcomm)
                                   (go again)
                                 (block not-syncd
                                   (setf (comm-data rcomm) (comm-data wcomm)
                                         (ref-cell-value performed) evnum
                                         (ref-cell-value (comm-performed wcomm))
                                         (comm-evnum wcomm))
                                   (mpcompat:mailbox-send (comm-mbox wcomm) t)
                                   (return t))))
                           ))))
         :suspend (lambda ()
                      (if (channel-valid ch)
                          (setf (channel-pending-readers ch)
                                (um:conc1 (cleanup-queue (channel-pending-readers ch))
                                       rcomm))
                        (decf (ref-cell-value nrvalid))))
         :result (lambda ()
                     (let ((ans (comm-data rcomm)))
                       (if (eq ans *no-data*)
                           (error "Event.receive")
                         ans)))
         )))))

(defun alwaysEvt (data)
  (make-instance 'comm-event
                 :behavior
  (lambda (performed mbox evnum nrvalid)
      (declare (ignore mbox nrvalid))
      (make-instance
       'behaviors
       :poll    (lambda ()
                  (setf (ref-cell-value performed) evnum)
                  t)
       :suspend (constantly nil)
       :result  (constantly data))
      )))

(defvar neverEvt
  (make-instance 'comm-event
                 :behavior
                 (lambda (performed mbox evnum nrvalid)
                   (declare (ignore performed mbox evnum nrvalid))
                   (make-instance 'behaviors
                                  :poll    (constantly nil)
                                  :suspend (constantly nil)
                                  :result  (lambda ()
                                             (error "neverEvt.result"))) )))

(defun cleanup-queue (q)
  ;; remove all communication opportunities already synchronized
  (delete-if 'already-syncd-p q))

;; -----------------------------------------------------
;; Reppy Combinators
;;
(defclass choice-event (event)
  ((choices  :accessor choice-list  :initarg :choices)))

(defun choose (&rest lst)
  (make-instance 'choice-event
                 :choices lst))

(defclass guarded-event (event)
  ((fn   :accessor guard-fn   :initarg :fn)
   (args :accessor guard-args :initarg :args)))

(defun guard (fn &rest args)
  (make-instance 'guarded-event
                 :fn fn
                 :args args))

(defclass wrapped-abort-event (event)
  ((evt  :accessor wrapped-event  :initarg :event)
   (fn   :accessor abort-fn       :initarg :fn)
   (args :accessor abort-args     :initarg :args)))

(defmethod wrap-abort ((ev event) fn &rest args)
  (make-instance 'wrapped-abort-event
                 :event ev
                 :fn    fn
                 :args  args))

(defmethod wrap ((ev comm-event) fn &rest args)
  (let ((sav (comm-behavior ev)))
    (setf (comm-behavior ev)
          (lambda (performed mbox evnum nrvalid)
              (let ((bev (funcall sav performed mbox evnum nrvalid)))
                (make-instance 'behaviors
                               :poll    (bev-poll bev)
                               :suspend (bev-suspend bev)
                               :result  (lambda ()
                                            (apply fn
                                                   (funcall
                                                    (bev-result bev))
                                                   args))
                               ))))
    ev))

(defmethod wrap ((ev choice-event) fn &rest args)
  (choose (mapcar (lambda (ev)
                      (apply 'wrap ev fn args))
                  (choice-list ev))))

(defmethod wrap ((ev wrapped-abort-event) fn &rest args)
  (apply 'wrap-abort
         (apply 'wrap (wrapped-event ev) fn args)
         (abort-fn ev)
         (abort-args ev)))

(defmethod wrap ((ev guarded-event) fn &rest args)
  (guard (lambda ()
             (apply 'wrap
                    (apply (guard-fn ev) (guard-args ev))
                    fn args))
         ))

(defun scramble-array (v)
  (let ((len (length v)))
    (if (zerop len)
        (error "Event.choose"))
    (do ((ix (1- len) (1- ix)))
        ((zerop ix))
      (let ((jx (random (1+ ix)))
            (tmp (aref v ix)))
        (setf (aref v ix) (aref v jx)
              (aref v jx) tmp)))
    v))

(defmethod flatten-event (abort-list accum accum-abort (ev comm-event))
  (values (cons (make-instance 'bev-cell
                               :bev (comm-behavior ev)
                               :abort-ids abort-list)
                accum)
          accum-abort))

(defvar *id-counter* 0)
(defun gen-id ()
  (incf *id-counter*))

(defmethod flatten-event (abort-list accum accum-abort (ev wrapped-abort-event))
  (let ((id (gen-id)))
    (flatten-event (cons id abort-list) accum
                   (cons (make-instance 'abort-cell
                                        :id   id
                                        :fn   (abort-fn ev)
                                        :args (abort-args ev))
                         accum-abort)
                   (wrapped-event ev))))

(defmethod flatten-event (abort-list accum accum-abort (ev guarded-event))
  (flatten-event abort-list accum accum-abort
                 (apply (guard-fn ev) (guard-args ev))))

(defmethod flatten-event (abort-list accum accum-abort (ev choice-event))
  (labels
      ((flatten-list (accum accum-abort lst)
          (if (endp lst)
              (values accum accum-abort)
            (multiple-value-bind (accumx accum-abortx)
                (flatten-event abort-list accum accum-abort
                               (car lst))
              (flatten-list accumx accum-abortx (cdr lst)))
            )))
    (flatten-list accum accum-abort (choice-list ev))))

              
(defun do-aborts (abort-env)
  (dolist (cell abort-env)
    (apply (abort-cell-fn cell) (abort-cell-args cell))))

(defun do-selective-aborts (abort-env genev performed)
  (let ((nabort-ids (bev-cell-abort-ids (aref genev (ref-cell-value performed)))))
    (do-aborts (delete-if (lambda (cell)
                              (member (abort-cell-id cell) nabort-ids))
                          abort-env))
    ))

(defvar *masterlock* (mpcompat:make-lock :name "Reppy Master Lock"))

(defun prep-bevs (genev)
  (let* ((performed (make-ref nil))
         (mbox      (mpcompat:make-mailbox :size 1))
         (len       (length genev))
         (nrvalid   (make-ref len))
         (bev       (make-array len)))
    (dotimes (ix len)
      (setf (aref bev ix)
            (funcall (bev-cell-bev (aref genev ix))
                     performed mbox ix nrvalid)))
    (values performed mbox bev nrvalid)))

(defun channel-ready (bev)
  (funcall (bev-poll bev)))

(defun suspend-bev (bev)
  (funcall (bev-suspend bev)))

(defun get-result (bevs performed)
  (funcall (bev-result (aref bevs (ref-cell-value performed)))))

(defvar *die* #(:die)) ;; unique value

(defun sync-wait (mbox)
  (when (eq *die* (mpcompat:mailbox-read mbox))
    (mpcompat:process-kill (mpcompat:current-process))))

(defun basic-sync (abort-env genev)
  (multiple-value-bind (performed mbox bev nrvalid)
      (prep-bevs genev)
    (let ((needs-wait nil))
      (mpcompat:with-lock (*masterlock*)
        (unless (some 'channel-ready bev)
          ;; suspend until a communication takes place
          (map nil 'suspend-bev bev)
          (when (zerop (ref-cell-value nrvalid))
            (mpcompat:mailbox-send mbox *die*))
          (setf needs-wait t)))
      (when needs-wait
        (sync-wait mbox)))
    (let ((ans (get-result bev performed)))
      (do-selective-aborts abort-env genev performed)
      ans)))

(defun basic-poll (abort-env genev)
  (multiple-value-bind (performed mbox bev nrvalid)
      (prep-bevs genev)
    (declare (ignore mbox nrvalid))
    (let ((ready nil))
      (mpcompat:with-lock (*masterlock*)
        (setf ready (some 'channel-ready bev)))
      (if ready
          (progn
            (do-selective-aborts abort-env genev performed)
            (values (get-result bev performed) t))
        (progn
          (setf (ref-cell-value performed) t)  ;; mark all as sync'd
          (do-aborts abort-env)
          nil))
      )))

(defmethod poll ((ev event))
  (multiple-value-bind (evl abort-env)
      (flatten-event nil nil nil ev)
    (basic-poll abort-env (scramble-array (coerce evl 'vector)))
    ))

(defmethod sync ((ev event))
  (multiple-value-bind (evl abort-env)
      (flatten-event nil nil nil ev)
    (basic-sync abort-env (scramble-array (coerce evl 'vector)))
    ))

(defun select (&rest evl)
  (sync (apply 'choose evl)))

(defmethod send ((ch channel) item)
  (sync (sendEvt ch item)))

(defmethod recv ((ch channel))
  (sync (recvEvt ch)))

(defun sendNack (nackCh)
  (let* ((evt       (sendEvt nackCh t))
         (performed (make-ref nil))
         (mbox      (mpcompat:make-mailbox))
         (nrvalid   (make-ref 1))
         (beh       (funcall (comm-behavior evt)
                             performed mbox 0 nrvalid)))
    (mpcompat:with-lock (*masterlock*)
      (unless (funcall (bev-poll beh))
        (funcall (bev-suspend beh))))
    ))

(defmethod discard-channel ((ch channel))
  (mpcompat:with-lock (*masterlock*)
    (setf (channel-valid ch) nil)
    (labels
        ((poll (q)
           (dolist (comm q)
             (unless (already-syncd-p comm)
               (if (zerop (decf (ref-cell-value
                                 (comm-nrvalid comm))))
                   (mpcompat:mailbox-send (comm-mbox comm) *die*)))
             )))
      (poll (channel-pending-readers ch))
      (poll (channel-pending-writers ch)))
    ))

(defun with-nack (fn)
  (guard (lambda () 
	     (let ((nackCh (make-channel)))
	       (wrap-abort
	        (wrap (funcall fn (recvEvt nackCh))
		      (lambda (x)
		          (discard-channel nackCh)
		          x))
	        (lambda ()
		    (sendNack nackCh)))
               ))))

#|
(defun tst ()
  (let ((ch (make-channel))
        (ans (make-channel)))
    (spawn (lambda ()
             (sync (choose (wrap (recvEvt ch) (lambda (x) (send ans (list 1 x))))
                           (wrap (recvEvt ch) (lambda (y) (send ans (list 2 y))))))))
    (send ch 15)
    (recv ans)))

(defun tst ()
  (let ((ch1 (make-channel))
        (ch2 (make-channel))
        (ans (make-channel)))
    (spawn (lambda ()
             (send ans (list 1 (sync (choose (recvEvt ch1) (recvEvt ch2)))))))
    (spawn (lambda ()
             (send ans (list 2 (sync (choose (recvEvt ch2) (recvEvt ch1)))))))
    (spawn (lambda () (sync (sendEvt ch1 15))))
    (spawn (lambda () (sync (sendEvt ch2 16))))
    (recv ans)))
    

|#
