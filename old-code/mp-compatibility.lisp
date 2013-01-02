;; mp-compatibility.lisp
;; --------------------------------------------------------------------------------------
;; Compatibility layer for Lispworks, Allegro, OS X, and Win32, Mulit-Processing Primitives
;;
;; Copyright (C) 2008 by SpectroDynamics, LLC. All rights reserved.
;;
;; DM/SD  08/08
;; --------------------------------------------------------------------------------------

;; --------------------------------------------------
(in-package #:mp-compatibility)
;; --------------------------------------------------
;; Compatibility Layer

#+(OR :LISPWORKS :ALLEGRO :CLOZURE)
(defun current-process ()
  "Get the current Lisp process."
  mp:*current-process*)

;; --------------------------------------------------------------------------

(defun process-name (proc)
  (mp:process-name proc))

(defun set-process-name (proc name)
  (setf (mp:process-name proc) name))

;; --------------------------------------------------------------------------

#|
#-:CLOZURE
(defun process-plist (proc)
  "Return the property list for the indicated Lisp process."
  #+:LISPWORKS (mp:process-plist proc)
  #+:ALLEGRO   (sys::process-property-list proc))
|#

#-:CLOZURE
(defun get-process-plist-entry (proc key &optional default)
  "Set the property named by key in the process' property list to val"
  #+:LISPWORKS5 (getf (mp:process-plist proc) key default)
  #+:LISPWORKS6 (mp:process-property key proc default)
  #+:ALLEGRO    (getf (sys::process-property-list proc) key default))

#-:CLOZURE
(defun set-process-plist-entry (proc key val)
  "Set the property named by key in the process' property list to val"
  #+:LISPWORKS5 (setf (getf (mp:process-plist proc) key) val)
  #+:LISPWORKS6 (setf (mp:process-property key proc) val)
  #+:ALLEGRO    (setf (getf (sys::process-property-list proc) key) val))

;; --------------------------------------------------------------------------

#+:CLOZURE
(defvar *process-plists* (make-hash-table :weak :key :test 'eq))

#|
#+:CLOZURE
(defun process-plist (proc)
  "Return the property list for the indicated Lisp process."
  (gethash proc *process-plists*))
|#

;; TBD -- get-process-plist-entry

#+:CLOZURE
(defun set-process-plist-entry (proc key val)
  (um:if-let (lst (process-plist proc))
	     (setf (getf lst key) val)
	     (setf (gethash proc *process-plists*) (list key val))))

;; --------------------------------------------------------------------------

#+:LISPWORKS
(defun process-run-function (name flags proc &rest args)
  "Spawn a new Lisp thread and run the indicated function with inital args."
  (apply #'mp:process-run-function name flags proc args))

#+(OR :ALLEGRO :CLOZURE)
(defun process-run-function (name flags proc &rest args)
  "Spawn a new Lisp thread and run the indicated function with inital args."
  (declare (ignore flags))
  (apply #'mp:process-run-function name proc args))

;; --------------------------------------------------------------------------

#+(OR :LISPWORKS :ALLEGRO :CLOZURE)
(defun process-kill (proc)
  "Kill the indicated Lisp process."
  (mp:process-kill proc))

;; --------------------------------------------------------------------------

#+(OR :LISPWORKS :ALLEGRO :CLOZURE)
(defun process-interrupt (proc fn &rest args)
  "Interrupt the indicated Lisp process to have it perform a function."
  (apply #'mp:process-interrupt proc fn args))

;; --------------------------------------------------------------------------

(defmacro without-preemption (&body body)
  "Perform the body forms without preemption."
  `(#+:LISPWORKS mp:without-preemption
    #+:ALLEGRO   mp:without-scheduling
    #+:CLOZURE   mp:without-interrupts ,@body))

;; --------------------------------------------------------------------------
;; --------------------------------------------------------------------------

(defun make-lock (&key name important-p (safep t))
  "Make a Lisp lock."
  (declare (ignorable important-p safep))
  #+:LISPWORKS (mp:make-lock
                :name name
                :important-p important-p
                :safep safep)
  #+:ALLEGRO   (mp:make-process-lock
                :name name)
  #+:CLOZURE   (mp:make-lock name))

;; --------------------------------------------------------------------------

(defmacro with-lock ((lock &optional whostate timeout) &body body)
  "Wait for lock available, then execute the body while holding the lock."
  #+:LISPWORKS `(mp:with-lock (,lock ,whostate ,timeout) ,@body)
  #+:ALLEGRO   `(mp:with-process-lock (,lock :whostate ,whostate
                                             :timeout  ,timeout)
                                      ,@body)
  #+:CLOZURE  `(do-with-lock ,lock ,whostate ,timeout (lambda () ,@body)))

#+:CLOZURE
(defun do-with-lock (lock whostate timeout fn)
  (if timeout
      (and
       (do-grab-lock-with-timeout lock whostate timeout)
       (unwind-protect
	    (funcall fn)
	 (mp:release-lock lock)))
      (mp:with-lock-grabbed (lock) (funcall fn))
      ))

;; --------------------------------------------------------------------------

(defun lock-owner (lock)
  (declare (ignorable lock))
  #+:LISPWORKS (mp:lock-owner lock)
  #+:ALLEGRO   (mp:process-lock-locker lock)
  #+:CLOZURE   (error "lock-owner unimplemented"))

;; --------------------------------------------------------------------------

(defun process-lock (lock &optional whostate timeout)
  #+:LISPWORKS (mp:process-lock lock whostate timeout)
  #+:ALLEGRO   (mp:process-lock lock mp:*current-process* whostate timeout)
  #+:CLOZURE   (do-grab-lock-with-timeout lock whostate timeout))

#+:CLOZURE
(defun do-grab-lock-with-timeout (lock whostate timeout)
  (if timeout
       (or (mp:try-lock lock)
	   (mp:process-wait-with-timeout whostate
					 (round
					  (* timeout mp:*ticks-per-second*))
					 #'mp:try-lock (list lock)))
       (mp:grab-lock lock)))

;; --------------------------------------------------------------------------

(defun process-unlock (lock)
  #+:LISPWORKS (mp:process-unlock lock)
  #+:ALLEGRO   (mp:process-unlock lock mp:*current-process*)
  #+:CLOZURE   (mp:release-lock lock))

;; --------------------------------------------------------------------------

(defun make-mailbox (&key size)
  "Make a Lisp mailbox."
  (declare (ignorable size))
  #+:LISPWORKS (mp:make-mailbox :size size)
  #+:ALLEGRO   (make-instance 'mp:queue)
  #+:CLOZURE   (mrmb:create))

;; --------------------------------------------------------------------------

(defun mailbox-send (mbox msg)
  "Send a message to a Lisp mailbox."
  #+:LISPWORKS (mp:mailbox-send mbox msg)
  #+:ALLEGRO   (mp:enqueue mbox msg)
  #+:CLOZURE   (mrmb:send msg mbox))

;; --------------------------------------------------------------------------

#+:LISPWORKS
(defun mailbox-read (mbox &optional timeout)
  "Wait with timeout for a message to arrive at the Lisp mailbox and return it.
A null timeout means wait forever."
  (mp:mailbox-read mbox "Waiting for mail" timeout))

#+:ALLEGRO
(defun mailbox-read (mbox &optional timeout)
  "Wait with timeout for a message to arrive at the Lisp mailbox and return it.
A null timeout means wait forever."
  (if (mp:queue-empty-p mbox)
      (if timeout
          (if (sys::process-wait-with-timeout "Waiting on mailbox" timeout
                                              (lambda () 
                                                (not (mp:queue-empty-p mbox))))
              (mp:dequeue mbox))
        (mp:dequeue mbox :wait t))
    (mp:dequeue mbox)))

#+:CLOZURE
(defun mailbox-read (mbox &optional timeout)
  (mrmb:receive mbox timeout))

;; --------------------------------------------------------------------------

(defun mailbox-empty? (mbox)
  "Check if the Lisp mailbox is empty. Return generalized T/F."
  #+:LISPWORKS (mp:mailbox-empty-p mbox)
  #+:ALLEGRO   (mp:queue-empty-p mbox)
  #+:CLOZURE   (mrmb:is-empty mbox))

;; --------------------------------------------------------------------------

(defun process-wait (wait-reason wait-fn &rest wait-args)
  #+:LISPWORKS
  (apply #'mp:process-wait wait-reason wait-fn wait-args)
  #+:ALLEGRO
  (apply #'mp:process-wait wait-reason wait-fn wait-args)
  #+:CLOZURE
  (apply #'mp:process-wait wait-reason wait-fn wait-args))

;; --------------------------------------------------------------------------

(defun process-wait-with-timeout (wait-reason timeout
				  &optional wait-fn &rest wait-args)
  #+:LISPWORKS
  (apply #'mp:process-wait-with-timeout wait-reason timeout wait-fn wait-args)
  #+:ALLEGRO
  (if timeout
      (apply #'mp:process-wait-with-timeout wait-reason timeout wait-fn wait-args)
    (progn
      (apply #'mp:process-wait wait-reason wait-fn wait-args)
      t))
  #+:CLOZURE
  (apply #'mp:process-wait-with-timeout wait-reason
	 (round (* timeout mp:*ticks-per-second*))
	 wait-fn wait-args))

;; --------------------------------------------------------------------------

(defun generate-uuid ()
  #+(AND :LISPWORKS :MACOSX)  (uuid:byte-array-to-uuid (uuidgen:generate))
  #+(AND :LISPWORKS :WIN32)   (uuid:make-v1-uuid)
  #+:ALLEGRO (uuid:make-v1-uuid)
  #+:CLOZURE (uuid:make-v4-uuid))

;; --------------------------------------------------------------------------

(um:defmacro! critical (&body body)
  `(let ((,g!lock #.(make-lock :name "CriticalSection")))
     (with-lock (,g!lock)
       ,@body)))

