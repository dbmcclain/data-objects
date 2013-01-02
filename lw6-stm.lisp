
(in-package :lw6-stm)

(defmacro def-var (name &rest slots)
  `(defstruct ,name
     (%%modification-count 0)
     (%%lock  (mpcompat:make-lock))
     ,@slots))

(editor:setup-indent "def-var" 1)

(defun get-struct-arg (struct-name lambda-list)
  (or (car (find struct-name
                 (remove-if (complement 'consp) lambda-list)
                 :key 'cadr))
      (error "No ~A specializer in lambda list: ~A" struct-name lambda-list)))

(um:defmacro! def-accessor (method-name struct-name lambda-list &body body)
  (let ((mod-name  (intern (um:mkstr struct-name :-%%MODIFICATION-COUNT)))
        (arg       (get-struct-arg struct-name lambda-list)))

    (multiple-value-bind (decls forms)
        (um:separate-decls-and-body body)

      `(defmethod ,method-name ,lambda-list
         ,@decls
         (loop
          (sys:with-modification-check-macro ,g!not-changed (,mod-name ,arg)
            (let ((,g!ans (multiple-value-list (progn ,@forms))))
              (when (,g!not-changed)
                (return (values-list ,g!ans))))))) )))

(editor:setup-indent "def-accessor" 3 2)

(defmacro def-mutator (method-name struct-name lambda-list &body body)
  (let ((lock-name (intern (um:mkstr struct-name :-%%LOCK)))
        (mod-name  (intern (um:mkstr struct-name :-%%MODIFICATION-COUNT)))
        (arg       (get-struct-arg struct-name lambda-list)))
    
    (multiple-value-bind (decls forms)
        (um:separate-decls-and-body body)
      
      `(defmethod ,method-name ,lambda-list
         ,@decls
         (mpcompat:with-spinlock ((,lock-name ,arg))
           (sys:with-modification-change (,mod-name ,arg)
             (progn
               ,@forms)))) )))

(editor:setup-indent "def-mutator" 3 2)

;; -----------------------------------------------------------------------------

(um:defmacro! with-accessing ((arg stm-type) &body body)
  (let ((mod-name (intern (um:mkstr stm-type :-%%MODIFICATION-COUNT))))
    `(loop
      (sys:with-modification-check-macro ,g!not-changed (,mod-name ,arg)
        (let ((,g!ans (multiple-value-list (progn ,@body))))
          (when (,g!not-changed)
            (return (values-list ,g!ans)))))) ))

(editor:setup-indent "with-accessing" 1)

(defmacro with-mutation ((arg stm-type) &body body)
  (let ((mod-name  (intern (um:mkstr stm-type :-%%MODIFICATION-COUNT)))
        (lock-name (intern (um:mkstr stm-type :-%%LOCK))))
    `(mpcompat:with-spinlock ((,lock-name ,arg))
       (sys:with-modification-change (,mod-name ,arg)
         (progn
           ,@body))) ))

(editor:setup-indent "with-mutation" 1)
