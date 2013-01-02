;; rb-trees.lisp -- Immutable Functional Red-Black Trees
;; --------------------------------------------------------------------------------------
;; Binary tree storage for unique ordered items, plus Maps, FIFO Queues, and Stacks
;;
;; Copyright (C) 2008 by SpectroDynamics, LLC. All rights reserved.
;;
;; DM/SD  08/08
;; --------------------------------------------------------------------------------------

;; ------------------------------------------------------------------------
(in-package :sets-internal)
;; ------------------------------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)
  (import '(sets:height
            sets:add
            sets:min-elt
            sets:remove-min-elt)))

;; ----------------------------------------------------------------
;; Sets are represented by balanced binary trees
;; The heights of children differ by at most 2
;; Tree nodes are quadruples (l v r h) where:
;;   - l = left child
;;   - v = value
;;   - r = right child
;;   - h = height
;; ----------------------------------------------------------------

;; create - create a tree node with left son l, value v, and right son r.
;; We must have all elements of l < v < all elements of r.
;; l and r must be balanced and have a height difference <= 2

(defun create (l v r &optional (hl (height l)) (hr (height r)))
  #f
  (declare (fixnum hl hr))
  (list l v r (the fixnum (1+ (max hl hr)))))


(defun invalid-argument (fn-name)
  (error "Invalid argument in: ~A" fn-name))

;; bal - same as create, but performs one step of rebalancing if necessary
;; assumes l and r balanced and height difference <= 3

(defun bal (l v r)
  #f
  (flet ((invalid-arg ()
           (invalid-argument "Set:bal")))
    (let ((hl (height l))
          (hr (height r)))
      (declare (fixnum hl hr))
      (cond ((> hl (the fixnum (+ 2 hr)))
             (um:match l
               ((ll lv lr _)
                (if (>= (the fixnum (height ll)) (the fixnum (height lr)))
                    (create ll lv (create lr v r))
                  (um:match lr
                    ((lrl lrv lrr _)
                     (create (create ll lv lrl) lrv (create lrr v r)))
                    (_ (invalid-arg))
                    )))

               (_ (invalid-arg))
               ))

            ((> hr (the fixnum (+ 2 hl)))
             (um:match r
               ((rl rv rr _)
                (if (>= (the fixnum (height rr)) (the fixnum (height rl)))
                    (create (create l v rl) rv rr)
                  (um:match rl
                    ((rll rlv rlr _)
                     (create (create l v rll) rlv (create rlr rv rr)))
                    (_ (invalid-arg))
                    )))
               
               (_ (invalid-arg))
               ))

            (t (create l v r hl hr))
            ))))
            
;; join -- same as create and bal, but no assumptions are made on the
;; relative heights of l and r

(defun join (l v r)
  #f
  (um:match2 l r
    (() _  (add v r))
    (_ ()  (add v l))
    ((ll lv lr lh) (rl rv rr rh)
     (cond ((> lh (+ 2 rh)) (bal ll lv (join lr v r)))
           ((> rh (+ 2 lh)) (bal (join l v rl) rv rr))
           (t (create l v r))
           ))))

(defun not-found ()
  (error "Not found"))

;; merge -- merge two trees l and r into one.
;; All elements of l must precede the elements of r
;; Assume height difference <= 2

(defun merge (t1 t2)
  #f
  (um:match2 t1 t2
    (() _  t2)
    (_  () t1)
    (_  _  (bal t1 (min-elt t2) (remove-min-elt t2)))
    ))

;; concat - merge two trees l and r into one.
;; All elements of l must precede the elements of r.
;; No assumptions on the heights of l and r.

(defun concat (t1 t2)
  #f
  (um:match2 t1 t2
    (() _  t2)
    (_  () t1)
    (_  _  (join t1 (min-elt t2) (remove-min-elt t2)))
    ))

(defun cons-enum (s e)
  #f
  (um:match s
    (()        e)
    ((l v r _) (cons-enum l (list v r e)))
    ))

;; ------------------------------------------------------------------------
(in-package :sets)
;; ------------------------------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)
  (import '(sets-internal:create
            sets-internal:bal
            sets-internal:join
            sets-internal:concat
            sets-internal:cons-enum
            sets-internal:not-found
            sets-internal:invalid-argument))

  (shadowing-import '(sets-internal:merge)))

;; --------------------------------------------

(defun height (node)
  (um:match node
    ((_ _ _ h) h)
    (()        0)))

;; add - insertion of one element
(defun add (x node)
  #f
  (um:nlet addx ((node node))
    (um:match node
      (()         (values (singleton x) t))
      ((l v r h)  (if (eql x v)
                      node
                    (let ((c (ord:compare x v)))
                      (cond ((zerop c) (list l x r h))
                            ;; to support maps (see below)
                            ;; cause new map value to be substituted for old value
                            ((minusp c)
                             (multiple-value-bind (new-left needs-rebal) (addx l)
                               (cond ((eq l new-left) node)
                                     (needs-rebal     (values (bal new-left v r) t))
                                     (t               (create new-left v r))
                                     )))
                            (t
                             (multiple-value-bind (new-right needs-rebal) (addx r)
                               (cond ((eq r new-right) node)
                                     (needs-rebal      (values (bal l v new-right) t))
                                     (t                (create l v new-right))
                                     )))
                            ))))
      )))

(defun min-elt (node)
  #f
  (um:match node
    (()         (not-found))
    ((() v _ _) v)
    ((l  _ _ _) (min-elt l))
    ))

(defun max-elt (node)
  #f
  (um:match node
    (()         (not-found))
    ((_ v () _) v)
    ((_ _ r  _) (max-elt r))
    ))

;; remove-min-elt - remove the smallest element of the set
;; also useful for priority-queues

(defun remove-min-elt (node)
  #f
  (um:match node
    (()         (invalid-argument "Sets-internal::remove-min-elt"))
    ((() _ r _) r)
    ((l  v r _) (bal (remove-min-elt l) v r))
    ))

;; split - split x s returns a triple (l present r) where
;; - l is the set of elements of s that are < x
;; - r is the set of elements of s that are > x
;; - present is false if s contains no element equal to x
;;   or true if s contains an element equal to x

(defun split (x tree)
  #f
  (um:match tree
    (()        (list nil nil nil))
    ((l v r _) (let ((c (ord:compare x v)))
                 (cond ((zerop c) (list l t r))
                       ((minusp c)
                        (destructuring-bind (ll pres rl) (split x l)
                          (list ll pres (join rl v r)) ))
                       (t (destructuring-bind (lr pres rr) (split x r)
                            (list (join l v lr) pres rr) ))
                       )))
    ))

(defun empty ()
  nil)

(defun is-empty (tree)
  (null tree))

(defun mem (x tree)
  #f
  (um:match tree
    (()        nil)
    ((l v r _) (let ((c (ord:compare x v)))
                 (or (zerop c)
                     (mem x (if (minusp c) l r)))
                 ))
    ))

(defun singleton (x)
  (list nil x nil 1))

(defun remove (x tree)
  #f
  (um:match tree
    (()        nil)
    ((l v r _) (let ((c (ord:compare x v)))
                 (cond ((zerop c) (merge l r))
                       ((minusp c) (bal (remove x l) v r))
                       (t (bal l v (remove x r)))
                       )))
    ))

(defun union (s1 s2)
  #f
  (um:match2 s1 s2
    (() _  s2)
    (_  () s1)
    ((l1 v1 r1 h1) (l2 v2 r2 h2)
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
           ))
    ))

(defun inter (s1 s2)
  #f
  (um:match2 s1 s2
    (() _  nil)
    (_  () nil)
    ((l1 v1 r1 _) _ 
     (destructuring-bind (l2 ans r2) (split v1 s2)
       (if ans
           (join (inter l1 l2) v1 (inter r1 r2))
         (concat (inter l1 l2) (inter r1 r2)) )
       ))
    ))

(defun diff (s1 s2)
  #f
  (um:match2 s1 s2
    (() _  nil)
    (_  () nil)
    ((l1 v1 r1 _) _
     (destructuring-bind (l2 ans r2) (split v1 s2)
       (if ans
           (concat (diff l1 l2) (diff r1 r2))
         (join (diff l1 l2) v1 (diff r1 r2)) )
       ))
    ))

(defun compare (s1 s2)
  #f
  (um:nlet-tail iter ((e1 (cons-enum s1 nil))
                      (e2 (cons-enum s2 nil)))
    (um:match2 e1 e2
      (() ()   0)
      (() _   -1)
      (_  ()   1)
      ((v1 r1 e1) (v2 r2 e2)
       (let ((c (ord:compare v1 v2)))
         (if (zerop c)
             (iter (cons-enum r1 e1)
                   (cons-enum r2 e2))
           c)))
      )))

(defun equal (s1 s2)
  #f
  (zerop (compare s1 s2)))

(defun subset (s1 s2)
  #f
  (um:match2 s1 s2
    (() _         t)
    (_  ()        nil)
    ((l1 v1 r1 _) (l2 v2 r2 _)
     (let ((c (ord:compare v1 v2)))
       (cond ((zerop c) (and (subset l1 l2)
                             (subset r1 r2)))
             ((minusp c) (and (subset (list l1 v1 () 0) l2)
                              (subset r1 s2)))
             (t (and (subset (list () v1 r1 0) r2)
                     (subset l1 s2)))
             )))
    ))


(defun iter (fn s)
  #f
  (um:match s
    (()  nil)
    ((l v r _) 
     (iter fn l)
     (funcall fn v)
     (iter fn r))
    ))

(defun fold (fn s accu)
  #f
  (um:match s
    (()        accu)
    ((l v r _) (fold fn r (funcall fn v (fold fn l accu))))
    ))

(defun for-all (pred s)
  #f
  (um:match s
    (()        t)
    ((l v r _) (and (funcall pred v)
                    (for-all pred l)
                    (for-all pred r)))
    ))

(defun exists (pred s)
  #f
  (um:match s
    (()        nil)
    ((l v r _) (or (funcall pred v)
                   (exists pred l)
                   (exists pred r)))
    ))

(defun filter (pred s)
  #f
  (um:nlet filt ((accu nil)
                 (s    s))
    (um:match s
      (()        accu)
      ((l v r _) (filt (filt (if (funcall pred v)
                                 (add v accu)
                               accu)
                             l)
                       r))
      )))

(defun partition (pred s)
  #f
  (um:nlet part ((pair (list nil nil))
                 (s    s))
    (destructuring-bind (tp fp) pair
      (um:match s
        (()        pair)
        ((l v r _) (part (part (if (funcall pred v)
                                   (list (add v tp) fp)
                                 (list tp (add v fp)))
                               l)
                         r))
        ))))

(defun cardinal (s)
  #f
  (um:match s
    (()        0)
    ((l _ r _) (+ (cardinal l) 1 (cardinal r)))
    ))

(defun elements (s)
  #f
  (um:nlet iter ((accu nil)
                 (s    s))
    (um:match s
      (()        accu)
      ((l v r _) (iter (cons v (iter accu r)) l))
      )))

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
                                       (um:match tree
                                         (()          nil)
                                         ((() _ () _) nil)
                                         ((l  _ () _) (list l))
                                         ((() _ r  _) (list r))
                                         ((l  _ r  _) (list l r))))
                  
                  :print-function (lambda (node)
                                    (um:match node
                                      (()        "")
                                      ((_ v _ _) (format nil "~D" v))))
                  )))

#|
;; examine effects of constructing a tree in pure ascending or descending order
(view-set (let ((xt (sets:empty))) (dotimes (ix 100) (setf xt (sets:add ix xt))) xt))
(view-set (let ((xt (sets:empty))) (dotimes (ix 100) (setf xt (sets:add (- 100 ix) xt))) xt))
|#
;; -------------------------------------------------------------

;; --------------------------------------------
(in-package :maps)
;; --------------------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)
  (import '(sets-internal:cons-enum)))

;; --------------------------------------------

(defstruct map-cell
  key val)

(defmethod ord:compare ((a map-cell) (b map-cell))
  ;; for comparing two map cells
  ;; used by sets:add
  (ord:compare (map-cell-key a) (map-cell-key b)))

(defmethod ord:compare (a (b map-cell))
  ;; for comparing keys against map-cells
  (ord:compare a (map-cell-key b)))


(defun empty ()
  (sets:empty))

(defun is-empty (map)
  (sets:is-empty map))

(defun mem (x map)
  (sets:mem x map))

(defun remove (x map)
  (sets:remove x map))

(defun add (key val map)
  (sets:add (make-map-cell
             :key key
             :val val)
            map))

(defun find (key map &optional default)
  #f
  (um:match map
    (()    (values default nil))
    ((l v r _) (let ((c (ord:compare key (map-cell-key v))))
                 (cond ((zerop c) (values (map-cell-val v) t))
                       (t         (find key (if (minusp c) l r)))
                       )))
    ))

(defun compare (cmp map1 map2)
  #f
  (um:nlet-tail iter ((e1 (cons-enum map1 nil))
                      (e2 (cons-enum map2 nil)))
    (cond ((and (null e1)
                (null e2))
           0)
          
          ((null e1) -1)
          ((null e2)  1)
          (t (destructuring-bind (v1 r1 e1) e1
               (destructuring-bind (v2 r2 e2) e2
                 (let ((c (ord:compare (map-cell-key v1) (map-cell-key v2))))
                   (cond ((not (zerop c)) c)
                         (t (let ((c (funcall cmp (map-cell-val v1) (map-cell-val v2)) ))
                              (cond ((not (zerop c)) c)
                                    (t (iter (cons-enum r1 e1)
                                             (cons-enum r2 e2)))
                                    ))))))))
          )))

(defun equal (cmp map1 map2)
  #f
  (zerop (compare cmp map1 map2)))

(defun fold (f map accu)
  #f
  (um:match map
    (()        accu)
    ((l v r _) (fold f r
                     (funcall f
                              (map-cell-key v)
                              (map-cell-val v)
                              (fold f l accu))) )
    ))

(defun map (f map)
  #f
  (um:match map
    (()     nil)
    ((l v r h) (list
                (map f l)
                (make-map-cell
                 :key (map-cell-key v)
                 :val (funcall f (map-cell-val v)))
                (map f r)
                h))
    ))

(defun mapi (f map)
  #f
  (um:match map
    (()   nil)
    ((l v r h) (list
                (mapi f l)
                (let ((key (map-cell-key v)))
                  (make-map-cell
                   :key key
                   :val (funcall f key (map-cell-val v))))
                (mapi f r)
                h))
    ))

(defun iter (f map)
  #f
  (um:match map
    (()  nil)
    ((l v r _)
     (iter f l)
     (funcall f (map-cell-key v) (map-cell-val v))
     (iter f r))
    ))

;; -----------------------------------------------------------
;; RB-Queues -- Functional Queues done in RB-Trees to maximize
;; structure sharing

(defpackage :rbqueue
  (:use #:common-lisp)
  (:shadowing-import-from #:maps
   #:remove)
  (:import-from #:maps
   #:empty
   #:is-empty
   #:map-cell
   #:map-cell-key
   #:map-cell-val
   #:fold)
  (:shadow #:pop)
  (:export
   #:empty
   #:is-empty
   #:add
   #:pop
   #:pop-all
   #:transfer
   ))

;; --------------------------------------------
(in-package :rbqueue)
;; --------------------------------------------

(defun add (x q)
  #f
  (if (is-empty q)
      (maps:add 0 x q)
    (let ((cell (sets:min-elt q)))
      (maps:add (1- (map-cell-key cell)) x q))
    ))

(defun pop (q &optional (empty-error-p t) empty-val)
  #f
  (if (is-empty q)
      (if empty-error-p
          (error "Queue empty")
        (values q empty-val nil))
    (let ((cell (sets:max-elt q)))
      (values (remove cell q)
              (map-cell-val cell)
              t))
    ))

(defun pop-all (q &optional (empty-error-p t))
  #f
  (if (is-empty q)
      (when empty-error-p
        (error "Queue empty"))
    (fold (lambda (k v accu)
            (declare (ignore k))
            (cons v accu))
          q nil)))

(defun transfer (q1 q2)
  #f
  (um:foldl (lambda (q x)
              (add x q))
            q2 (pop-all q1)))
