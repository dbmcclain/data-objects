;; ord.lisp
;; --------------------------------------------------------------------------------------
;; Compare ordering of various types
;;
;; Copyright (C) 2008 by SpectroDynamics, LLC. All rights reserved.
;;
;; DM/SD  08/08
;; --------------------------------------------------------------------------------------

;; ------------------------------------------------------------------------
(in-package :ord)
;; ------------------------------------------------------------------------

(defmethod compare ((a real) (b real))
  #f
  (- a b))

(defmethod compare ((a character) (b character))
  #f
  (cond ((char= a b) 0)
        ((char< a b) -1)
        (t 1)))

(defmethod compare ((a string) (b string))
  #f
  (cond ((string= a b) 0)
        ((string< a b) -1)
        (t 1)))

(defmethod compare ((a symbol) (b symbol))
  #f
  (compare (symbol-name a) (symbol-name b)))

(defmethod compare ((a pathname) (b pathname))
  #f
  (compare (namestring a) (namestring b)))

(defstruct ci-char
  c)

(defmethod compare ((a ci-char) (b ci-char))
  #f
  (let ((ca (ci-char-c a))
        (cb (ci-char-c b)))
    (cond ((char-equal ca cb)  0)
          ((char-lessp ca cb) -1)
          (t 1))))

(defstruct ci-string
  s)

(defmethod compare ((a ci-string) (b ci-string))
  #f
  (let ((sa (ci-string-s a))
        (sb (ci-string-s b)))
    (cond ((string-equal sa sb)  0)
          ((string-lessp sa sb) -1)
          (t 1))))

;; ------------------------------------------

(defun compare< (a b)
  #f
  (minusp (the fixnum (compare a b))))

(defun compare<= (a b)
  #f
  (not (plusp (the fixnum (compare a b)))))

(defun compare= (a b)
  #f
  (zerop (the fixnum (compare a b))))

(defun compare>= (a b)
  #f
  (not (minusp (the fixnum (compare a b)))))

(defun compare> (a b)
  #f
  (plusp (the fixnum (compare a b))))

;; ------------------------------------------------------------------------
#|
(defmethod compare= (a b)
  (eql a b))

(defmethod compare= ((a number) (b number))
  (= a b))

(defmethod compare= ((a string) (b string))
  (string= a b))

(defstruct string-ci
  str)

(defmethod compare= ((a string-ci) (b string-ci))
  (string-equal (string-ci-str a) (string-ci-str b)))

(defmethod compare= ((a character) (b character))
  (char= a b))

(defstruct char-ci
  char)

(defmethod compare= ((a char-ci) (b char-ci))
  (char-equal (char-ci-char a) (char-ci-char b)))

(defmethod compare= ((a pathname) (b pathname))
  (pathname-match-p a b))

(defmethod compare= ((a list) (b list))
  (cond ((null a) (null b))
        ((null b) nil)
        (t        (and (compare= (first a) (first b))
                       (compare= (rest a)  (rest b))))
        ))

(defmethod compare= ((a array) (b array))
  (and (= (array-rank a) (array-rank b))
       (every '= (array-dimensions a) (array-dimensions b))
       (eql (adjustable-array-p a) (adjustable-array-p b))
       (eql (array-element-type a) (array-element-type b))
       (every 'compare=
              (make-array (array-total-size a)
                          :element-type (array-element-type a)
                          :displaced-to a
                          :displaced-index-offset 0)
              (make-array (array-total-size b)
                          :element-type (array-element-type b)
                          :displaced-to b
                          :displaced-index-offset 0))
       ))

(defmethod compare= ((a vector) (b vector))
  (and (= (length a) (length b))
       (eql (array-element-type a) (array-element-type b))
       (eql (adjustable-array-p a) (adjustable-array-p b))
       (eql (fill-pointer a) (fill-pointer b))
       (every 'compare= a b)))
|#
;; ------------------------------------------------------------------------

