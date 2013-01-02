;; rps.lisp
;; --------------------------------------------------------------------------------------
;; Reactive Programmng Style -- General purpose reactive programming through
;; explicit notifications and by implicit mutation notification.
;;
;; Copyright (C) 2008 by SpectroDynamics, LLC. All rights reserved.
;;
;; DM/SD  08/08
;; --------------------------------------------------------------------------------------

(in-package :common-lisp)

;; --------------------------------------------------------------------------------------
(in-package :rps)
;; --------------------------------------------------------------------------------------

(defparameter *current-rps-engine* nil)

(defstruct rps-engine
  (bq (queue:create))  ;; befores
  (aq (queue:create))) ;; afters
    
(defun rps-running-p ()
  (rps-engine-p *current-rps-engine*))

;; --------------------------------------------------------------------------------------

(defmacro with-rps (&body body)
  `(do-with-rps (lambda () ,@body)))

(defun do-with-rps (fn)
  (if (rps-running-p)
      (funcall fn)
    (let ((*current-rps-engine* (make-rps-engine)))
      (funcall fn)
      (do ((dfn (get-action) (get-action)))
          ((null dfn))
        (funcall dfn))
      )))

(defun get-action ()
  (or (queue:pop (rps-engine-bq *current-rps-engine*) nil)
      (queue:pop (rps-engine-aq *current-rps-engine*) nil)))

;; --------------------------------------------------------------------------------------
;; Often times an action will wish to enqueue further actions

(defun enqueue-action (fn)
  (with-rps
    (queue:add fn (rps-engine-bq *current-rps-engine*))
    ))

(defmacro enqueue (&body body)
  `(enqueue-action (lambda ()
                     ,@body)))

(defun enqueue-after-action (fn)
  (with-rps
    (queue:add fn (rps-engine-aq *current-rps-engine*))
    ))

(defmacro enqueue-after (&body body)
  `(enqueue-after-action (lambda ()
                           ,@body)))

;; --------------------------------------------------------------------------------------
;; Environment maps from objects to dep-cells that point to those objects
;; Dep-cells are constructed on demand when a dependent or an observer is
;; declared against the object.

(defun make-environment ()
  #+:LISPWORKS
  (make-hash-table :test 'eq :weak-kind :key)
  #+:CLOZURE
  (make-hash-table :test 'eq :weak :key)
  #+:SBCL
  (make-hash-table :test 'eq :weakness :key)
  #+:ALLEGRO
  (make-hash-table :test 'eq))

(defparameter *environment* (make-environment))

(defmethod get-dependents (obj)
  (gethash obj *environment*))

(defmethod set-dependents (obj deps)
  (setf (gethash obj *environment*) deps))

(defmethod remove-dependents (obj)
  (remhash obj *environment*))

;; --------------------------------------------------------------------------------------

(defgeneric value-at  (obj &key &allow-other-keys))
(defgeneric basic-set-value (new-val place &key &allow-other-keys))

;; --------------------------------------------------------------------------------------

(defun value (x &rest args &key at)
  (if (null at)
      (value-of x)
    (apply #'value-at (value-of x) args)))

;; ------------------------------

(defmethod value-of (x)
  x)

(defmethod value-of ((sym symbol))
  (symbol-value sym))

;; ------------------------------

(defmethod value-at (x &key at)
  (error "can't dereference object: ~S at: ~S" x at))

(defmethod value-at ((ht hash-table) &key at default)
  (gethash at ht default))

(defmethod value-at ((seq sequence) &key at)
  (elt seq at))

(defmethod value-at ((vec vector) &key at)
  (aref vec at))

(defmethod value-at ((arr array) &key at)
  (cond ((consp at) (apply #'aref arr at))
        (t          (row-major-aref arr at))
        ))
         
(defmethod value-at ((obj standard-object) &key at)
  (slot-value obj at))

#+(OR :LISPWORKS :CLOZURE :SBCL)
(defmethod value-at ((obj structure-object) &key at)
  (slot-value obj at))

;; --------------------------------------------------------------------------------------

(defun set-value (new-val place &rest args)
  (um:if-let (deps (get-dependents place))
      (apply #'set-value-with-dependencies new-val place deps args)
    (apply #'basic-set-value new-val place args)))

;; --------------------------------------------------------------------------------------
;; Use (SETF (VALUE ... ) ...) to effect changes that permit dependent clauses
;; to notice the changes.

;; (defsetf value (place &rest args) (new-value)
;;    `(set-value ,new-value ,place ,@args))

(defun (setf value) (new-value place &rest args)
   (apply #'set-value new-value place args))

;; E.g., for slotted objects like strutures and class instances:
;; (SETF (VALUE obj :AT 'slot-name) new-val)
;;
;; For sequences (strings, vectors, lists)
;; (SETF (VALUE obj :AT index) new-val)
;;
;; For arrays
;; (SETF (VALUE obj :AT '(list-of-indices)) new-val)
;;
;; For symbols
;; (SETF (VALUE 'symbol) new-val)

;; --------------------------------------------------------------------------------------
;; Noticed-Mutable-Objects are faster than plain object because they hold their own
;; dependency lists. Plain objects require a hash-table lookup.

(defclass noticed-mutable-object ()
  ((deps   :accessor deps   :initform nil)
   (val    :accessor val    :initform nil  :initarg :val)))

(defun make-noticed-mutable-object (val)
  (make-instance 'noticed-mutable-object
                 :val val))

(defmethod value-of ((obj noticed-mutable-object))
  (val obj))

(defmethod basic-set-value (new-val (obj noticed-mutable-object) &rest args &key &allow-other-keys)
  (apply #'basic-set-value new-val (val obj) args))

(defmethod get-dependents ((obj noticed-mutable-object))
  (deps obj))

(defmethod set-dependents ((obj noticed-mutable-object) new-deps)
  (setf (deps obj) new-deps))

(defmethod remove-dependents ((obj noticed-mutable-object))
  (setf (deps obj) nil))

;; --------------------------------------------------------------------------------------
;; WITH-NOTICED-MUTATIONS -- used when we need to use setf subseq, replace, etc.
;; Any mutations outside of (setf (value ...))

(defmacro with-noticed-mutations ((obj &key at) &body body)
  `(do-with-noticed-mutations ,obj ,at (lambda () ,@body)))

(defun do-with-noticed-mutations (obj at fn)
  (with-rps
    (let ((old-val (value obj :at at)))
      (funcall fn)
      (enqueue-dependents obj at (get-dependents obj)
                          old-val (value obj :at at))
      )))

;; --------------------------------------------------------------------------------------

(defun set-value-with-dependencies (new-val place deps &key at)
  ;; changing some slot of object referred to by cell
  (with-rps
    (let ((old-val (value place :at at)))
      (basic-set-value new-val place :at at)
      (enqueue-dependents place at deps old-val new-val)
      )))

(defun enqueue-dependents (place at deps old-val new-val)
  (labels ((enqueue-these (alst at-sel)
             (um:when-let (atfns (assoc at-sel alst))
               (dolist (dfn-entry (cdr atfns))
                 (um:bind* ((:values (test dfn) (if (consp dfn-entry)
                                                    (um:bind* (((test . dfn) dfn-entry))
                                                      (values test dfn))
                                                  (values 'eql dfn-entry))))
                   (unless (funcall test old-val new-val)
                     (enqueue
                       (funcall dfn place at old-val new-val))
                     ))
                 )))
           (enqueue-all ()
             (when at
               (enqueue-these deps at))  ;; dependents of cell object at slot
             (enqueue-these deps nil)    ;; dependents of cell or cell object regardless of slot
             
             (let ((observers (get (type-of place) 'observers)))
               (when at
                 (enqueue-these observers at))   ;; observers of type at slot
               (enqueue-these observers nil))    ;; observers of type regardless of slot
             ))
    (enqueue-all)
    ))

;; --------------------------------------------------------------------------------------

(defmethod basic-set-value (new-val (place symbol) &key &allow-other-keys)
  (setf (symbol-value place) new-val))

(defmethod basic-set-value (new-val (place hash-table) &key at &allow-other-keys)
  (setf (gethash at place) new-val))

(defmethod basic-set-value (new-val (place sequence) &key at &allow-other-keys)
  (setf (elt place at) new-val))

(defmethod basic-set-value (new-val (place vector) &key at &allow-other-keys)
  (setf (aref place at) new-val))

(defmethod basic-set-value (new-val (place array) &key at &allow-other-keys)
  (cond ((consp at) (setf (apply #'aref place at) new-val))
        (t          (setf (row-major-aref place at) new-val))
        ))
  
(defmethod basic-set-value (new-val (place standard-object) &key at &allow-other-keys)
  (setf (slot-value place at) new-val))

#+(OR :LISPWORKS :CLOZURE :SBCL)
(defmethod basic-set-value (new-val (place structure-object) &key at &allow-other-keys)
  (setf (slot-value place at) new-val))

;; --------------------------------------------------------------------------------------
;; ephemeral cells have quiescent val = nil
;; updates to cell persist only until all notifications
;; have been sent, then the cell is reset to nil.
;; No notifications are sent on the reset.

(defclass ephemeral-cell (noticed-mutable-object)
  ())

(defun make-ephemeral-cell ()
  (make-instance 'ephemeral-cell))

(defmethod basic-set-value (new-val (cell ephemeral-cell) &key)
  (setf (val cell) new-val)
  (enqueue-after
    (setf (val cell) nil)))

;; --------------------------------------------------------------------------------------
;; Generic dependents list maintenance
;; Dependency lists are associations between the slot-name (or nil) and a list of
;; dependent function entries. A dependent function entry is either a function designator
;; (which gets called unless old-val is EQL to new-val), or else it is a dotted pair whose
;; car is the test to be applied to old-val and new-val, and the cdr is a function designator
;; to be called when old-val and new-val don't agree according to the test function.

(defun trim-atfns (atfns fn)
  (delete fn (cdr atfns) :key #'(lambda (entry)
                                  (if (consp entry)
                                      (cdr entry)
                                    entry))
          ))

(defun make-atfns-entry (fn test)
  (if test
      (cons test fn)
    fn))

(defun save-trimmed-alst (alst atfns setter remover)
  (um:if-let (trimmed-list (delete atfns alst))
      (funcall setter trimmed-list)
    (funcall remover)))

;; -----------------------

(defun add-dep (alst fn at test setter)
  (let ((atfns (assoc at alst)))
    (if atfns
        (setf (cdr atfns) (cons (make-atfns-entry fn test)
                                (trim-atfns atfns fn)))
      (funcall setter (acons at (list (make-atfns-entry fn test)) alst))
      )))

(defun rem-dep (alst fn at setter remover)
  (let ((atfns (assoc at alst)))
    (when atfns
      (um:if-let (trimmed-fns (trim-atfns atfns fn))
          (setf (cdr atfns) trimmed-fns)
        (save-trimmed-alst alst atfns setter remover)
        ))
    ))

(defun clr-dep (alst at setter remover)
  (let ((atfns (assoc at alst)))
    (when atfns
      (save-trimmed-alst alst atfns setter remover))
    ))

;; ------------------------------------------------------------------------------------
;; Dependents on specific objects and possibly on some slot of the object

(defun add-dependent (fn obj &key at test)
  (add-dep (get-dependents obj) fn at test
           (um:curry #'set-dependents obj)))

(defun remove-dependent (fn obj &key at)
  (rem-dep (get-dependents obj) fn at
           (um:curry #'set-dependents obj)
           (um:curry #'remove-dependents obj)))

(defun clear-dependents (obj &key at)
  (clr-dep (get-dependents obj) at
           (um:curry #'set-dependents obj)
           (um:curry #'remove-dependents obj)))

(defun clear-all-dependents (obj)
  (remove-dependents obj))

;; --------------------------------------------------------------------------------------
;; Observers against types (types denoted by symbols, as from type-of)

(defun get-observers (type-symbol)
  (get type-symbol 'observers))

(defun set-observers (type-symbol obs-list)
  (setf (get type-symbol 'observers) obs-list))

(defun remove-observers (type-symbol)
  (remprop type-symbol 'observers))

;; ----------------------------

(defun add-observer (fn type-symbol &key at test)
  (add-dep (get-observers type-symbol) fn at test
           (um:curry #'set-observers type-symbol)))

(defun remove-observer (fn type-symbol &key at)
  (rem-dep (get-observers type-symbol) fn at
           (um:curry #'set-observers type-symbol)
           (um:curry #'remove-observers type-symbol)))

(defun clear-observers (type-symbol &key at)
  (clr-dep (get-observers type-symbol) at
           (um:curry #'set-observers type-symbol)
           (um:curry #'remove-observers type-symbol)))

(defun clear-all-observers (type-symbol)
  (remove-observers type-symbol))

;; --------------------------------------------------------------------------------------
;; Notifications by name (including other parameters)

(defparameter *notification-table* (make-hash-table :test 'equalp))

(defun register-notification-action (name fn)
  (let ((lst (gethash name *notification-table*)))
    (setf (gethash name *notification-table*) (adjoin fn lst))))

(defun remove-notification-action (name fn)
  (let ((lst (gethash name *notification-table*)))
    (setf (gethash name *notification-table*) (delete fn lst))))

(defun clear-notification-actions (name)
  (remhash name *notification-table*))

(defun notify (name &rest notification)
  (dolist (fn (gethash name *notification-table*))
    (enqueue
      (apply fn name notification))))

;; --------------------------------------------------------------------------------------
#|

(defstruct motor
  status fuel-pump temp)

(defparameter *motor1* (make-motor
                        :status :on
                        :fuel-pump :open
                        :temp  0))

(defun tpr (str old-val new-val)
  (format t "~&~A changing from ~S to ~S~%" str old-val new-val))

(defun monitor-status (obj slot old-val new-val)
  (declare (ignore slot))
  (tpr "motor status" old-val new-val)
  (setf (value obj :at 'fuel-pump) (case new-val
                                     (:on  :open)
                                     (:off :closed))))

(defun monitor-fuel-pump (obj slot old-val new-val)
  (declare (ignore obj slot))
  (tpr "motor fuel-pump" old-val new-val))
  
(defun monitor-temp (obj slot old-val new-val)
  (declare (ignore slot))
  (tpr "motor temperature" old-val new-val)
  (setf (value obj :at 'status)
        (if (< (* (round new-val 0.05) 0.05) 100)
            :on :off)))

(add-dependent 'monitor-status    *motor1* :at 'status    :test 'eq)
(add-dependent 'monitor-fuel-pump *motor1* :at 'fuel-pump :test #'eq)
(add-dependent 'monitor-temp      *motor1* :at 'temp)

;; the following uses implicit RPS -- engine is instantiated as needed.
;; (but a series of consecutive changes will not be deferred)

(setf (value *motor1* :at 'temp) 0)
(dotimes (x 2)
  (dotimes (y 10)
    (let ((newtemp (+ 99 x (random 0.07) -.02))) 
      (setf (value *motor1* :at 'temp) newtemp))))

;; -------------------------------------------------------------------------------
;; This next version illustrates the importance of temporal separation of
;; function application.
;;
;; Had we just enclosed the above nested do-loop with WITH-RPS
;; then all of the setf's would occur at once, followed by the execution of the
;; resulting dependent clauses. Secondary clauses won't get the chance to run until
;; the first dependent clause gets its chance to run, which happens after the do-loops complete.

(with-rps
  (setf (value *motor1* :at 'temp) 0)
  (labels ((iterx (x)
             (when (< x 2)
               (itery x 0)))
           (itery (x y)
             (if (< y 10)
                 (enqueue-after
                   (let ((newtemp (+ 99 x (random 0.07) -.02))) 
                     (setf (value *motor1* :at 'temp) newtemp))
                   (itery x (1+ y)))
               (iterx (1+ x)))))
    (iterx 0)))

(with-rps
  ;; see what we mean?
  (setf (value *motor1* :at 'temp) 0)
  (dotimes (x 2)
    (dotimes (y 10)
      (let ((newtemp (+ 99 x (random 0.07) -.02))) 
        (setf (value *motor1* :at 'temp) newtemp)))))

|#

;; ------------------------------------------------------------------------
;; RPS-METACLASS and NOTICED-SLOTS-OBJECT
;; Can use normal setf accessor or setf slot in addition to setf value, as above

(defclass noticed-slots-metalevel-class (standard-class)
  ())

#+(OR :LISPWORKS :ALLEGRO)
(defmethod (setf clos:slot-value-using-class)
           (new-val (class noticed-slots-metalevel-class) object slot-name)
  ;; (format t "~&setf slot: ~A~%" slot-name)
  (with-rps
    (let ((old-val (slot-value object slot-name)))
      (call-next-method)
      (enqueue-dependents object slot-name
                          (get-dependents object)
                          old-val new-val)
      )))
  
#+(OR :CLOZURE :SBCL)
(defmethod (setf clos:slot-value-using-class)
           (new-val (class noticed-slots-metalevel-class) object slot-def)
  ;; (format t "~&setf slot: ~A~%" slot-def)
  (with-rps
    (let* ((slot-name (clos:slot-definition-name slot-def))
	   (old-val   (and (slot-boundp object slot-name)
			   (slot-value object slot-name))))
      (call-next-method)
      (when (slot-boundp object 'dependents)
	(unless (eq 'dependents slot-name)
	  (enqueue-dependents object slot-name
			      (get-dependents object)
			      old-val new-val))
	))))
  
;; ------------------------------------------------------------------------
;; NOTICED-SLOTS-OBJECT -- root class of objects whose slots are automatically
;; monitored for changes using normal setf slot or setf accessor, in addition to set value.
;;
;; Such objects have their own local dependency list. We make it into a vector of one element
;; which element is the dependency list. That way we don't ever directly modify the dep slot
;; and so we don't trigger monitoring of that particular slot.
;;
;; Normal slots of defined subclasses will be monitored. Be sure to use (:optimize-slot-access nil)
;; in all subclasses. Otherwise, only setf slot will be montiored, not setf accesssor.

(defmethod clos:validate-superclass ((class noticed-slots-metalevel-class)
                                     (super standard-class))
  t)

(defclass noticed-slots-root-class ()
  ((dependents  :accessor noticed-slots-dependents :initform (vector nil)))
  ;; (:optimize-slot-access nil)
  (:metaclass noticed-slots-metalevel-class))

(defmethod get-dependents ((obj noticed-slots-root-class))
  (aref (noticed-slots-dependents obj) 0))

(defmethod set-dependents ((obj noticed-slots-root-class) new-dependents)
  (setf (aref (noticed-slots-dependents obj) 0) new-dependents))

(defmethod remove-dependents ((obj noticed-slots-root-class))
  (setf (aref (noticed-slots-dependents obj) 0) nil))

;; ----------------------------------------------------------------------------------------

(defun ensure-persistent-root-superclass (class direct-superclasses)
  ;; ensure that any persistent classes
  ;; - as implied by using metaclass persistent-metalevel-class -
  ;; gets the persistent root class as one of its superclasses.
  
  (let ((root-class (find-class 'noticed-slots-root-class))
        (pobjs-mc   (find-class 'noticed-slots-metalevel-class)))
    (if (or (eq class root-class)
            (some (lambda (super)
                    (eq pobjs-mc (class-of super)))
                  direct-superclasses))
        direct-superclasses
      (append direct-superclasses (list root-class)))))

(defmethod initialize-instance :around ((class noticed-slots-metalevel-class) &rest all-keys
                                        &key direct-superclasses)
  (let ((new-direct-superclasses (ensure-persistent-root-superclass class direct-superclasses)))
    (apply #'call-next-method
           class
           :direct-superclasses new-direct-superclasses
           #+:LISPWORKS :optimize-slot-access #+:LISPWORKS nil
           all-keys)))

(defmethod reinitialize-instance :around ((class noticed-slots-metalevel-class) &rest all-keys
                                          &key direct-superclasses)
  (let ((new-direct-superclasses (ensure-persistent-root-superclass class direct-superclasses)))
    (apply #'call-next-method
           class
           :direct-superclasses new-direct-superclasses
           #+:LISPWORKS :optimize-slot-access #+:LISPWORKS nil
           all-keys)))

;; ----------------------------------------------------------------------------------------

(defmacro define-monitored-class (name superclass slots)
  `(defclass ,name ,(or superclass '(noticed-slots-root-class)) ,slots
     ;; (:optimize-slot-access nil)
     (:metaclass noticed-slots-metalevel-class)))

#+:LISPWORKS
(editor:setup-indent "define-monitored-class" 2 2)

;; ----------------------------------------------------------------------------------------
#| ;; test it out...

(define-monitored-class motor ()
  ((status    :accessor motor-status    :initform nil :initarg :status)
   (fuel-pump :accessor motor-fuel-pump :initform nil :initarg :fuel-pump)
   (temp      :accessor motor-temp      :initform nil :initarg :temp)))
          
(defclass motor ()
  ((status    :accessor motor-status    :initform nil :initarg :status)
   (fuel-pump :accessor motor-fuel-pump :initform nil :initarg :fuel-pump)
   (temp      :accessor motor-temp      :initform nil :initarg :temp))
  ;; (:optimize-slot-access nil)
  (:metaclass noticed-slots-metalevel-class))

(defparameter *motor1* (make-instance 'motor
                                      :status :on
                                      :fuel-pump :open
                                      :temp  0))

(defun tpr (str old-val new-val)
  (format t "~&~A changing from ~S to ~S~%" str old-val new-val))

(defun monitor-status (obj slot old-val new-val)
  (declare (ignore slot))
  (tpr "motor status" old-val new-val)
  (setf (motor-fuel-pump obj) (case new-val
                                (:on  :open)
                                (:off :closed))))

(defun monitor-fuel-pump (obj slot old-val new-val)
  (declare (ignore obj slot))
  (tpr "motor fuel-pump" old-val new-val))
  
(defun monitor-temp (obj slot old-val new-val)
  (declare (ignore slot))
  (tpr "motor temperature" old-val new-val)
  (setf (motor-status obj)
        (if (< (* (round new-val 0.05) 0.05) 100)
            :on :off)))

(add-dependent 'monitor-status    *motor1* :at 'status    :test 'eq)
(add-dependent 'monitor-fuel-pump *motor1* :at 'fuel-pump :test #'eq)
(add-dependent 'monitor-temp      *motor1* :at 'temp)

;; the following uses implicit RPS -- engine is instantiated as needed.
;; (but a series of consecutive changes will not be deferred)

(setf (motor-temp *motor1*) 0)
(dotimes (x 2)
  (dotimes (y 10)
    (let ((newtemp (+ 99 x (random 0.07) -.02))) 
      (setf (motor-temp *motor1*) newtemp))))

;; -------------------------------------------------------------------------------
;; This next version illustrates the importance of temporal separation of
;; function application.
;;
;; Had we just enclosed the above nested do-loop with WITH-RPS
;; then all of the setf's would occur at once, followed by the execution of the
;; resulting dependent clauses. Secondary clauses won't get the chance to run until
;; the first dependent clause gets its chance to run, which happens after the do-loops complete.

(with-rps
  (setf (value *motor1* :at 'temp) 0)
  (labels ((iterx (x)
             (when (< x 2)
               (itery x 0)))
           (itery (x y)
             (if (< y 10)
                 (enqueue-after
                   (let ((newtemp (+ 99 x (random 0.07) -.02))) 
                     (setf (value *motor1* :at 'temp) newtemp))
                   (itery x (1+ y)))
               (iterx (1+ x)))))
    (iterx 0)))

(with-rps
  ;; see what we mean?
  (setf (value *motor1* :at 'temp) 0)
  (dotimes (x 2)
    (dotimes (y 10)
      (let ((newtemp (+ 99 x (random 0.07) -.02))) 
        (setf (value *motor1* :at 'temp) newtemp)))))

|#