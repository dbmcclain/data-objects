
;; -------------------------------
(in-package #:simple-vstm)
;; -------------------------------

;; -------------------------
;; concurrent access and read-modify-write

(defstruct var
  val)

(defmacro rmw ((val v) &body body)
  `(do-rmw ,v (lambda (,val) ,@body)))

#+:LISPWORKS
(editor:setup-indent "rmw" 1)

(defun do-rmw (v fn)
  (loop for prev = (var-val v)
        for new  = (funcall fn prev)
        until (mpcompat:compare-and-swap (var-val v) prev new)))

