;; sum-types.lisp -- ML-style SUM types for Lisp
;; DM/RAL 02/09
;; ------------------------------------------------------------------


(defstruct sum-type
  type selector data)

(defun sum-type-of (x type)
  (and (sum-type-p x)
       (eq type (sum-type-type x))))

#|
(defun make-option (selector &optional data)
  (ecase selector
    (:NONE (make-sum-type
            :type     :OPTION
            :selector :NONE))
    (:SOME (make-sum-type
            :type     :OPTION
            :selector :SOME
            :data     data))
    ))

(defun option? (x)
  (sum-type-of x :OPTION))
|#

#|
(defparameter x (make-option :NONE))
(defparameter y (make-option :SOME 15))
|#

(defun check-args (nargs argslist msg)
  (unless (= nargs (length argslist))
    (error "Incorrect number of arguments for ~A" msg)))

(defmacro define-sum-type (name &rest models)
  (let ((gselector (gensym))
        (gdata     (gensym)))
    `(progn
       (defun ,(intern (um:mkstr name :?)) (,gselector)
         (sum-type-of ,gselector ',name))
       (defun ,(intern (um:mkstr name :-TYPE)) (,gselector)
         (assert (,(intern (um:mkstr name :?)) ,gselector))
         (sum-type-selector ,gselector))
       (defun ,(intern (um:mkstr name :-DATA)) (,gselector)
         (assert (,(intern (um:mkstr name :?)) ,gselector))
         (sum-type-data ,gselector))
       (defun ,(intern (um:mkstr :make- name)) (,gselector &rest ,gdata)
         (ecase ,gselector
           ,@(mapcar
              (lambda (model)
                (let ((selector (first model)))
                  `(,selector (make-sum-type
                               :type ',name
                               :selector ',selector
                               :data  ,(let ((nargs (length (cdr model))))
                                         `(progn
                                            (check-args ,nargs ,gdata 
                                                        ,(format nil "~A:~A" name selector))
                                            ,(cond ((= nargs 1) `(first ,gdata))
                                                   ((> nargs 1) gdata)))) ) )))
              models))))
    ))

;; -----------------------------------------------------------------

#|
(define-sum-type :OPTION
                 (:NONE)
                 (:SOME a))

(defparameter x (make-option :NONE))
(defparameter y (make-option :SOME 15))


(define-sum-type tree-node
                 (:empty)
                 (:node left val right height))

(um:match x
  ( #S(SUM-TYPE :TYPE :OPTION :SELECTOR :SOME :DATA data) data)
  ( #S(SUM-TYPE :TYPE :OPTION :SELECTOR :NONE) 'yes))
|#

#|
(defstruct none)
(defstruct some
  val)
(defstruct tree-node
  left val right height)

(defparameter x (make-none))
(defparameter y (make-some :val 15))
(defparameter z (make-tree-node :left nil :val 32 :right (make-tree-node :val 13 :height 1) :height 2))

(um:match z
  ( #S(TREE-NODE :LEFT left :VAL val :RIGHT right :HEIGHT h)
   (list left val right h)))

(um:match y
  ( #S(SOME :VAL v) v)
  ( #S(NONE) 'yes))

(um:match z
  (#T(TREE-NODE :RIGHT #T(TREE-NODE :VAL x)) x))
|#
