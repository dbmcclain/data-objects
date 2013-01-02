;; rb-trees-hdstm.lisp -- lock-free MPU safe Red-Black Trees using Herlihy DSTM
;; --------------------------------------------------------------------------------------
;; Binary tree storage for unique ordered items, plus Maps, FIFO Queues, and Stacks
;;
;; Copyright (C) 2010 by SpectroDynamics, LLC. All rights reserved.
;;
;; DM/SD  01/10
;; --------------------------------------------------------------------------------------

(defpackage :ordx
  (:use #:common-lisp)
  (:export
   #:compare
   #:compare<
   #:compare<=
   #:compare=
   #:compare>=
   #:compare>
   #:make-ci-char
   #:make-ci-string
   ))

(defpackage :sets-internalx
  (:use #:common-lisp)
  (:shadow #:merge)
  (:export
   #:make-rb-tree
   #:rb-tree-p
   #:rb-tree-l
   #:rb-tree-v
   #:rb-tree-r
   #:rb-tree-h
   #:create
   #:height
   #:add
   #:min-elt
   #:remove-min-elt
   #:bal
   #:join
   #:merge
   #:concat
   #:cons-enum
   #:not-found
   #:invalid-argument
   #:lr
   #:lvr
   #:lvrh
   ))

(defpackage :setsx
  (:use #:common-lisp)
  (:import-from #:sets-internalx
   #:create
   #:bal
   #:join
   #:concat
   #:cons-enum
   #:not-found
   #:invalid-argument
   #:lr
   #:lvr
   #:lvrh
   #:make-rb-tree
   #:rb-tree-p
   #:rb-tree-l
   #:rb-tree-v
   #:rb-tree-r
   #:rb-tree-h
   #:height
   #:add
   #:min-elt
   #:remove-min-elt)
  (:shadowing-import-from #:sets-internalx
   #:merge)
  (:shadow #:equal #:remove #:union)
  (:export
   #:height
   #:empty
   #:is-empty
   #:mem
   #:add
   #:singleton
   #:remove
   #:remove-min-elt
   #:union
   #:inter
   #:diff
   #:compare
   #:equal
   #:subset
   #:iter
   #:fold
   #:for-all
   #:exists
   #:filter
   #:split
   #:partition
   #:cardinal
   #:elements
   #:min-elt
   #:max-elt
   #:choose
   ))

(defpackage :mapsx
  (:use #:common-lisp)
  (:shadow #:find #:equal #:map #:remove)
  (:import-from #:sets-internalx
   #:lr
   #:lvr
   #:lvrh)
  (:export
   #:empty
   #:is-empty
   #:add
   #:find
   #:remove
   #:mem
   #:iter
   #:map
   #:mapi
   #:fold
   #:compare
   #:equal
   ))

;; ------------------------------------------------------------------------
(in-package :ordx)
;; ------------------------------------------------------------------------

(defmethod compare ((a real) (b real))
  (- a b))

(defmethod compare ((a character) (b character))
  (cond ((char= a b) 0)
        ((char< a b) -1)
        (t 1)))

(defmethod compare ((a string) (b string))
  (cond ((string= a b) 0)
        ((string< a b) -1)
        (t 1)))

(defmethod compare ((a symbol) (b symbol))
  (compare (symbol-name a) (symbol-name b)))

(defmethod compare ((a pathname) (b pathname))
  (compare (namestring a) (namestring b)))

(defstruct ci-char
  c)

(defmethod compare ((a ci-char) (b ci-char))
  (let ((ca (ci-char-c a))
        (cb (ci-char-c b)))
    (cond ((char-equal ca cb)  0)
          ((char-lessp ca cb) -1)
          (t 1))))

(defstruct ci-string
  s)

(defmethod compare ((a ci-string) (b ci-string))
  (let ((sa (ci-string-s a))
        (sb (ci-string-s b)))
    (cond ((string-equal sa sb)  0)
          ((string-lessp sa sb) -1)
          (t 1))))

;; ------------------------------------------

(defun compare< (a b)
  (minusp (compare a b)))

(defun compare<= (a b)
  (not (plusp (compare a b))))

(defun compare= (a b)
  (zerop (compare a b)))

(defun compare>= (a b)
  (not (minusp (compare a b))))

(defun compare> (a b)
  (plusp (compare a b)))

;; ------------------------------------------------------------------------
(in-package :sets-internalx)
;; ------------------------------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)
  (import '(setsx:height
            setsx:add
            setsx:min-elt
            setsx:remove-min-elt)))

;; ----------------------------------------------------------------
;; Sets are represented by balanced binary trees
;; The heights of children differ by at most 2
;; Tree nodes are quadruples (l v r h) where:
;;   - l = left child
;;   - v = value
;;   - r = right child
;;   - h = height
;; ----------------------------------------------------------------

(defclass rb-tree ()
  ((l   :accessor rb-tree-l  :initform nil  :initarg :l)
   (v   :accessor rb-tree-v  :initform nil  :initarg :v)
   (r   :accessor rb-tree-r  :initform nil  :initarg :r)
   (h   :accessor rb-tree-h  :initform 1    :initarg :h)))

(defun make-rb-tree (&rest args)
  (apply 'make-instance 'rb-tree args))

(defmethod rb-tree-p (x)
  (declare (ignore x))
  nil)

(defmethod rb-tree-p ((tree rb-tree))
  tree)

;; create - create a tree node with left son l, value v, and right son
;; r.  We must have all elements of l < v < all elements of r.  l and
;; r must be balanced and have a height difference <= 2

(defun create (l v r &optional (hl (height l)) (hr (height r)))
  (make-rb-tree
   :l l
   :v v
   :r r
   :h (1+ (max hl hr))))


(defun invalid-argument (fn-name)
  (error "Invalid argument in: ~A" fn-name))

;; ------------------------------------------------------------------
;; destructuring macros

(defmacro lr ((l r) tree &body body)
  (let ((gtree (gensym (symbol-name :tree-))))
    `(let ((,gtree ,tree))
       (let ((,l  (rb-tree-l ,gtree))
             (,r  (rb-tree-r ,gtree)))
         ,@body)) ))

(editor:setup-indent "lr" 2)


(defmacro lvr ((l v r) tree &body body)
  (let ((gtree (gensym (symbol-name :tree-))))
    `(let ((,gtree ,tree))
       (let ((,l  (rb-tree-l ,gtree))
             (,v  (rb-tree-v ,gtree))
             (,r  (rb-tree-r ,gtree)))
         ,@body)) ))

(editor:setup-indent "lvr" 2)


(defmacro lvrh ((l v r h) tree &body body)
  (let ((gtree (gensym (symbol-name :tree-))))
    `(let ((,gtree ,tree))
       (let ((,l  (rb-tree-l ,gtree))
             (,v  (rb-tree-v ,gtree))
             (,r  (rb-tree-r ,gtree))
             (,h  (rb-tree-h ,gtree)))
         ,@body)) ))

(editor:setup-indent "lvrh" 2)

;; -------------------------------------------------

;; bal - same as create, but performs one step of rebalancing if
;; necessary. Assumes l and r balanced and height difference <= 3

(defun bal (l v r)
  (flet ((invalid-arg ()
           (invalid-argument "Set:bal")))
    (let ((hl (height l))
          (hr (height r)))
      (cond ((> hl (+ 2 hr))
             (cond ((rb-tree-p l)
                    (lvr (ll lv lr) l
                      (if (>= (height ll) (height lr))
                          (create ll lv (create lr v r))
                        ;; else
                        (cond ((rb-tree-p lr)
                               (lvr (lrl lrv lrr) lr
                                 (create (create ll lv lrl) lrv (create lrr v r))))

                              (t (invalid-arg)) )) ))
                   (t (invalid-arg)) ))
            
            ((> hr (+ 2 hl))
             (cond ((rb-tree-p r)
                    (lvr (rl rv rr) r
                      (if (>= (height rr) (height rl))
                          (create (create l v rl) rv rr)
                        ;; else
                        (cond ((rb-tree-p rl)
                               (lvr (rll rlv rlr) rl
                                 (create (create l v rll) rlv (create rlr rv rr))))
                              
                              (t (invalid-arg)) )) ))
               
                   (t (invalid-arg)) ))
            
            (t (create l v r hl hr)) )) ))
            
;; join -- same as create and bal, but no assumptions are made on the
;; relative heights of l and r

(defun join (l v r)
  (cond ((null l) (add v r))
        ((null r) (add v l))
        (t (lvrh (ll lv lr lh) l
             (lvrh (rl rv rr rh) r
               (cond ((> lh (+ 2 rh)) (bal ll lv (join lr v r)))
                     ((> rh (+ 2 lh)) (bal (join l v rl) rv rr))
                     (t (create l v r))
                     )))) ))

(defun not-found ()
  (error "Not found"))

;; merge -- merge two trees l and r into one.  All elements of l must
;; precede the elements of r Assume height difference <= 2

(defun merge (t1 t2)
  (cond ((null t1) t2)
        ((null t2) t1)
        (t (bal t1 (min-elt t2) (remove-min-elt t2)))
        ))

;; concat - merge two trees l and r into one.  All elements of l must
;; precede the elements of r.  No assumptions on the heights of l and
;; r.

(defun concat (t1 t2)
  (cond ((null t1) t2)
        ((null t2) t1)
        (t (join t1 (min-elt t2) (remove-min-elt t2)))
        ))

(defun cons-enum (s e)
  (cond ((null s) e)
        (t (lvr (l v r) s
             (cons-enum l (list v r e))))
        ))

;; ------------------------------------------------------------------------
(in-package :setsx)
;; ------------------------------------------------------------------------

(defun height (node)
  (cond ((null node) 0)
        (t           (rb-tree-h node)) ))

;; add - insertion of one element
(defun add (x node)
  (labels ((addx (node)
             (cond ((null node) (values (singleton x) t))
                   (t (lvrh (l v r h) node
                        (if (eql x v)
                            node
                          (let ((c (ordx:compare x v)))
                            (cond ((zerop c) (make-rb-tree
                                              :l l
                                              :v x
                                              :r r
                                              :h h))
                                  ;; to support maps (see below) cause
                                  ;; new map value to be substituted
                                  ;; for old value
                                  ((minusp c)
                                   (multiple-value-bind (new-left needs-rebal)
                                       (addx l)
                                     (cond ((eq l new-left) node)
                                           (needs-rebal     (values (bal new-left v r) t))
                                           (t               (create new-left v r))
                                           )))
                                  (t
                                   (multiple-value-bind (new-right needs-rebal)
                                       (addx r)
                                     (cond ((eq r new-right) node)
                                           (needs-rebal      (values (bal l v new-right) t))
                                           (t                (create l v new-right))
                                           )))
                                  )))))
                   )))
    (addx node)))

(defun min-elt (node)
  (cond ((null node) (not-found))
        ((null (rb-tree-l node)) (rb-tree-v node))
        (t     (min-elt (rb-tree-l node)))
        ))

(defun max-elt (node)
  (cond ((null node) (not-found))
        ((null (rb-tree-r node)) (rb-tree-v node))
        (t (max-elt (rb-tree-r node)))
        ))

;; remove-min-elt - remove the smallest element of the set also useful
;; for priority-queues

(defun remove-min-elt (node)
  (cond ((null node) (invalid-argument "Sets-internal::remove-min-elt"))
        ((null (rb-tree-l node)) (rb-tree-r node))
        (t (lvr (l v r) node
             (bal (remove-min-elt l) v r)))
        ))

;; split - split x s returns a triple (l present r) where
;; - l is the set of elements of s that are < x
;; - r is the set of elements of s that are > x
;; - present is false if s contains no element equal to x
;;   or true if s contains an element equal to x

(defun split (x tree)
  (cond ((null tree) (list nil nil nil))
        (t (lvr (l v r) tree
             (let ((c (ordx:compare x v)))
               (cond ((zerop c) (list l t r))
                     ((minusp c)
                      (destructuring-bind (ll pres rl) (split x l)
                        (list ll pres (join rl v r)) ))
                     (t (destructuring-bind (lr pres rr) (split x r)
                          (list (join l v lr) pres rr) ))
                     ))))
        ))

(defun empty ()
  nil)

(defun is-empty (tree)
  (null tree))

(defun mem (x tree)
  (cond ((null tree) nil)
        (t (lvr (l v r) tree
             (let ((c (ordx:compare x v)))
               (or (zerop c)
                   (mem x (if (minusp c) l r)))
               )))
        ))

(defun singleton (x)
  (make-rb-tree
   :v x))

(defun remove (x tree)
  (cond ((null tree) nil)
        (t (lvr (l v r) tree
             (let ((c (ordx:compare x v)))
               (cond ((zerop c) (merge l r))
                     ((minusp c) (bal (remove x l) v r))
                     (t (bal l v (remove x r)))
                     ))))
        ))

(defun union (s1 s2)
  (cond ((null s1) s2)
        ((null s2) s1)
        (t (lvrh (l1 v1 r1 h1) s1
             (lvrh (l2 v2 r2 h2) s2
               (cond ((>= h1 h2)
                      (if (= h2 1)
                          (add v2 s1)
                        (destructuring-bind (l2 _ r2) (split v1 s2)
                          (declare (ignore _))
                          (join (union l1 l2) v1 (union r1 r2)) )))
                     (t (if (= h1 1)
                            (add v1 s2)
                          (destructuring-bind (l1 _ r1) (split v2 s1)
                            (declare (ignore _))
                            (join (union l1 l2) v2 (union r1 r2)) )))
                     )))
           )))

(defun inter (s1 s2)
  (cond ((null s1) nil)
        ((null s2) nil)
        (t (lvr (l1 v1 r1) s1
             (destructuring-bind (l2 ans r2) (split v1 s2)
               (if ans
                   (join (inter l1 l2) v1 (inter r1 r2))
                 (concat (inter l1 l2) (inter r1 r2)) )
               )))
        ))

(defun diff (s1 s2)
  (cond ((null s1) nil)
        ((null s2) nil)
        (t (lvr (l1 v1 r1) s1
             (destructuring-bind (l2 ans r2) (split v1 s2)
               (if ans
                   (concat (diff l1 l2) (diff r1 r2))
                 (join (diff l1 l2) v1 (diff r1 r2)) )
               )))
        ))

(defun compare (s1 s2)
  (tagbody
   again
   (let ((e1 (cons-enum s1 nil))
         (e2 (cons-enum s2 nil)))
     (return-from compare
       (cond ((and (null e1) (null e2)) 0)
             ((null e1)      -1)
             ((null e2)       1)
             (t (destructuring-bind (v1 r1 ee1) e1
                  (destructuring-bind (v2 r2 ee2) e2
                    (let ((c (ordx:compare v1 v2)))
                      (if (zerop c)
                          (progn
                            (setf e1 (cons-enum r1 ee1)
                                  e2 (cons-enum r2 ee2))
                            (go again))
                        ;; else
                        c)) )))
             )) )))

(defun equal (s1 s2)
  (zerop (compare s1 s2)))

(defun subset (s1 s2)
  (cond ((null s1) t)
        ((null s2) nil)
        (t (lvr (l1 v1 r1) s1
             (lvr (l2 v2 r2) s2
               (let ((c (ordx:compare v1 v2)))
                 (cond ((zerop c) (and (subset l1 l2)
                                       (subset r1 r2)))
                       ((minusp c) (and (subset (make-rb-tree
                                                 :l l1
                                                 :v v1)
                                                l2)
                                        (subset r1 s2)))
                       (t (and (subset (make-rb-tree
                                        :v v1
                                        :r r1)
                                       r2)
                               (subset l1 s2)))
                       )))
             )) ))

(defun iter (fn s)
  (cond ((null s) nil)
        (t        (lvr (l v r) s
                    (iter fn l)
                    (funcall fn v)
                    (iter fn r))
                  ) ))

(defun fold (fn s accu)
  (cond ((null s) accu)
        (t       (lvr (l v r) s
                   (fold fn r (funcall fn v (fold fn l accu)))))
        ))

(defun for-all (pred s)
  (cond ((null s) t)
        (t        (lvr (l v r) s
                    (and (funcall pred v)
                         (for-all pred l)
                         (for-all pred r))))
        ))

(defun exists (pred s)
  (cond ((null s) nil)
        (t        (lvr (l v r) s
                    (or (funcall pred v)
                        (exists pred l)
                        (exists pred r))))
        ))

(defun filter (pred s)
  (labels ((filt (accu s)
             (cond ((null s)  accu)
                   (t         (lvr (l v r) s
                                (filt (filt (if (funcall pred v)
                                                (add v accu)
                                              accu)
                                            l)
                                      r)))
                   )))
    (filt nil s)))


(defun partition (pred s)
  (labels ((part (pair s)
             (destructuring-bind (tp fp) pair
               (cond ((null s) pair)
                     (t        (lvr (l v r) s
                                 (part (part (if (funcall pred v)
                                                 (list (add v tp) fp)
                                               (list tp (add v fp)))
                                             l)
                                       r)))
                     ))))
    (part (list nil nil) s)))


(defun cardinal (s)
  (cond ((null s) 0)
        (t     (lr (l r) s
                 (+ (cardinal l) 1 (cardinal r))))
        ))

(defun elements (s)
  (labels ((iter (accu s)
             (cond ((null s)  accu)
                   (t         (lvr (l v r) s
                                (iter (cons v (iter accu r)) l)))
                   )))
    (iter nil s)))

(defun choose (s)
  (min-elt s))


;; -------------------------------------------------------------

#|
(defun make-tree (&optional (tree (sets:empty)))
  (if (= (sets:height tree) 10)
      tree
    (make-tree (sets:add (random 16384) tree))))

#+:LISPWORKS
(capi:contain
 (make-instance 'capi:graph-pane
                :roots (list xtt)
                
                :children-function (lambda (tree)
                                     (cond ((and (null (first tree))
                                                 (null (third tree)))
                                            nil)
                                           ((null (first tree))
                                            (list (list nil #\x nil) (third tree)))

                                           ((null (third tree))
                                            (list (first tree) (list nil #\x nil)))
                                           
                                           (t (list (first tree)
                                                    (third tree)))
                                           ))
                
                :print-function (lambda (node)
                                  (format nil "~A" (second node)))
                ))
|#

#+:LISPWORKS
(defun view-set (s)
  (capi:contain
   (make-instance 'capi:graph-pane
                  :roots (list s)

                  :children-function (lambda (tree)
                                       (cond ((null tree) nil)
                                             (t (lr (l r) tree
                                                  (cond ((and (null l) (null r)) nil)
                                                        ((null r) (list l))
                                                        ((null l) (list r))
                                                        (t        (list l r)) ))) ))
                  
                  :print-function (lambda (node)
                                    (cond ((null node) "")
                                          (t (format nil "~D" (rb-tree-v node))) ))
                  )))

#|
;; examine effects of constructing a tree in pure ascending or
;; descending order
(view-set (let ((xt (setsx:empty))) (dotimes (ix 100) (setf xt (setsx:add ix xt))) xt))
(view-set (let ((xt (setsx:empty))) (dotimes (ix 100) (setf xt (setsx:add (- 100 ix) xt))) xt))
|#
;; -------------------------------------------------------------

;; --------------------------------------------
(in-package :mapsx)
;; --------------------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)
  (import '(sets-internalx:cons-enum)))

;; --------------------------------------------

(defclass map-cell ()
  ((key  :accessor map-cell-key  :initform nil :initarg :key)
   (val  :accessor map-cell-val  :initform nil :initarg :val)))


(defun make-map-cell (&rest args)
  (apply 'make-instance 'map-cell args))

(defmethod ordx:compare ((a map-cell) (b map-cell))
  ;; for comparing two map cells
  ;; used by setsx:add
  (ordx:compare (map-cell-key a) (map-cell-key b)))

(defmethod ordx:compare (a (b map-cell))
  ;; for comparing keys against map-cells
  (ordx:compare a (map-cell-key b)))


(defun empty ()
  (setsx:empty))

(defun is-empty (map)
  (setsx:is-empty map))

(defun mem (x map)
  (setsx:mem x map))

(defun remove (x map)
  (setsx:remove x map))

(defun add (key val map)
  (setsx:add (make-map-cell
             :key key
             :val val)
            map))

(defun find (key map &optional default)
  (cond ((null map) (values default nil))
        (t  (lvr (l v r) map
              (let ((c (ordx:compare key (map-cell-key v))))
                (cond ((zerop c) (values (map-cell-val v) t))
                      (t         (find key (if (minusp c) l r)))
                      ))))
        ))

(defun compare (cmp map1 map2)
  (tagbody
   again
   (let ((e1 (cons-enum map1 nil))
         (e2 (cons-enum map2 nil)))

     (return-from compare
       (cond ((and (null e1)
                   (null e2))
              0)
          
             ((null e1) -1)
             ((null e2)  1)
             (t (destructuring-bind (v1 r1 ee1) e1
                  (destructuring-bind (v2 r2 ee2) e2
                    (let ((c (ordx:compare (map-cell-key v1) (map-cell-key v2))))
                      (cond ((not (zerop c)) c)
                            (t (let ((c (funcall cmp (map-cell-val v1) (map-cell-val v2)) ))
                              (cond ((not (zerop c)) c)
                                    (t (setf e1 (cons-enum r1 ee1)
                                             e2 (cons-enum r2 ee2))
                                       (go again)) ))) ))))) )))))

(defun equal (cmp map1 map2)
  (zerop (compare cmp map1 map2)))

(defun fold (f map accu)
  (cond ((null map) accu)
        (t   (lvr (l v r) map
               (fold f r
                     (funcall f
                              (map-cell-key v)
                              (map-cell-val v)
                              (fold f l accu))) ))
        ))

(defun map (f map)
  (cond ((null map) nil)
        (t  (lvrh (l v r h) map
              (list
               (map f l)
               (make-map-cell
                :key (map-cell-key v)
                :val (funcall f (map-cell-val v)))
               (map f r)
               h)))
        ))

(defun mapi (f map)
  (cond ((null map) nil)
        (t  (lvrh (l v r h) map
              (list
               (mapi f l)
               (let ((key (map-cell-key v)))
                 (make-map-cell
                  :key key
                  :val (funcall f key (map-cell-val v))))
               (mapi f r)
               h)))
        ))

(defun iter (f map)
  (cond ((null map) nil)
        (t  (lvr (l v r) map
              (iter f l)
              (funcall f (map-cell-key v) (map-cell-val v))
              (iter f r)))
        ))

