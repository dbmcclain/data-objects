;; rb-trees.lisp
;; --------------------------------------------------------------------------------------
;; Binary tree storage for unique ordered items, plus Maps, FIFO Queues, and Stacks
;;
;; Copyright (C) 2008 by SpectroDynamics, LLC. All rights reserved.
;;
;; DM/SD  08/08
;; --------------------------------------------------------------------------------------

;; ------------------------------------------------------------------------
(in-package :rb-tree)
;; ------------------------------------------------------------------------

;; ----------------------------------------------------------------
;; Sets are represented by balanced binary trees
;; The heights of children differ by at most 2
;; Tree nodes are quadruples (l v r h) where:
;;   - l = left child
;;   - v = value
;;   - r = right child
;;   - h = height
;; ----------------------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defstruct empty-tree)
  (defconstant +empty-tree+ (make-empty-tree))
  
  (defstruct node
    (left +empty-tree+)
    value
    (right +empty-tree+)
    height) )

(deftype tree ()
  '(or empty-tree
       node))

;; create - create a tree node with left son l, value v, and right son r.
;; We must have all elements of l < v < all elements of r.
;; l and r must be balanced and have a height difference <= 2

(defun create (l v r &optional (hl (height l)) (hr (height r)))
  (make-node
   :left   l
   :value  v
   :right  r
   :height (1+ (max hl hr))))


(defun invalid-argument (fn-name)
  (error "Invalid argument in: ~A" fn-name))

;; bal - same as create, but performs one step of rebalancing if necessary
;; assumes l and r balanced and height difference <= 3

(defun bal (l v r)
  (flet ((invalid-arg ()
           (invalid-argument "Set:bal")))
    (let ((hl (height l))
          (hr (height r)))
      (cond ((> hl (+ 2 hr))
             (um:match l
               (#T(node :left ll :value lv :right lr)
                (if (>= (height ll) (height lr))
                    (create ll lv (create lr v r))
                  (um:match lr
                    (#T(node :left lrl :value lrv :right lrr)
                     (create (create ll lv lrl) lrv (create lrr v r)))
                    (_ (invalid-arg))
                    )))

               (_ (invalid-arg))
               ))

            ((> hr (+ 2 hl))
             (um:match r
               (#T(node :left rl :value rv :right rr)
                (if (>= (height rr) (height rl))
                    (create (create l v rl) rv rr)
                  (um:match rl
                    (#T(node :left rll :value rlv :right rlr)
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
  (um:match2 l r
    (#T(empty-tree) #T(tree) (add v r))
    (#T(tree) #T(empty-tree) (add v l))
    (#T(node :left ll :value lv :right lr :height lh)
       #T(node :left rl :value rv :right rr :height rh)
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
  (um:match2 t1 t2
    (#T(empty-tree) #T(tree) t2)
    (#T(tree) #T(empty-tree) t1)
    (#T(tree) #T(tree)
        (bal t1 (min-elt t2) (remove-min-elt t2)))
    ))

;; concat - merge two trees l and r into one.
;; All elements of l must precede the elements of r.
;; No assumptions on the heights of l and r.

(defun concat (t1 t2)
  (um:match2 t1 t2
    (#T(empty-tree) #T(tree) t2)
    (#T(tree) #T(empty-tree) t1)
    (#T(tree) #T(tree)
       (join t1 (min-elt t2) (remove-min-elt t2)))
    ))

(defun cons-enum (s e)
  (um:match s
    (#T(empty-tree)  e)
    (#T(node :left l :value v :right r) (cons-enum l (list v r e)))
    ))

;; --------------------------------------------

(defun height (node)
  (um:match node
    (#T(empty-tree)     0)
    (#T(node :height h) h)
    ))

;; add - insertion of one element
(defun add (x node)
  (um:nlet addx ((node node))
    (um:match node
      (#T(empty-tree) (values (singleton x) t))
      (#T(node :left l :value v :right r :height h)
         (if (eql x v)
             node
           (let ((c (ord:compare x v)))
             (cond ((zerop c) (make-node
                               :left   l
                               :value  x
                               :right  r
                               :height h))
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
  (um:match node
    (#T(empty-tree) (not-found))
    (#T(node :left #T(empty-tree) :value v) v)
    (#T(node :left l) (min-elt l))
    ))

(defun max-elt (node)
  (um:match node
    (#T(empty-tree) (not-found))
    (#T(node :value v :right #T(empty-tree)) v)
    (#T(node :right r) (max-elt r))
    ))

;; remove-min-elt - remove the smallest element of the set
;; also useful for priority-queues

(defun remove-min-elt (node)
  (um:match node
    (#T(empty-tree) (invalid-argument "Sets-internal::remove-min-elt"))
    (#T(node :left #T(empty-tree) :right r) r)
    (#T(node :left l :value v :right r) (bal (remove-min-elt l) v r))
    ))

;; split - split x s returns a triple (l present r) where
;; - l is the set of elements of s that are < x
;; - r is the set of elements of s that are > x
;; - present is false if s contains no element equal to x
;;   or true if s contains an element equal to x

(defun split (x tree)
  (um:match tree
    (#T(empty-tree) (list (empty) nil (empty)))
    (#T(node :left l :value v :right r)
       (let ((c (ord:compare x v)))
         (cond ((zerop c) (list l t r))
               ((minusp c)
                (destructuring-bind (ll pres rl) (split x l)
                  (list ll pres (join rl v r)) ))
               (t (destructuring-bind (lr pres rr) (split x r)
                    (list (join l v lr) pres rr) ))
               )))
    ))

(defun empty ()
  +empty-tree+)

(defun is-empty (tree)
  (um:match tree
    (#T(empty-tree) t)
    (#T(node)       nil)
    ))

(defun mem (x tree)
  (um:match tree
    (#T(empty-tree) nil)
    (#T(node :left l :value v :right r)
       (let ((c (ord:compare x v)))
         (or (zerop c)
             (mem x (if (minusp c) l r)))
         ))
    ))

(defun singleton (x)
  (make-node
   :value  x
   :height 1))

(defun remove (x tree)
  (um:match tree
    (#T(empty-tree) (empty))
    (#T(node :left l :value v :right r)
       (let ((c (ord:compare x v)))
         (cond ((zerop c) (merge l r))
               ((minusp c) (bal (remove x l) v r))
               (t (bal l v (remove x r)))
               )))
    ))

(defun union (s1 s2)
  (um:match2 s1 s2
    (#T(empty-tree) #T(tree) s2)
    (#T(tree) #T(empty-tree) s1)
    (#T(node :left l1 :value v1 :right r1 :height h1)
       #T(node :left l2 :value v2 :right r2 :height h2)
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
  (um:match2 s1 s2
    (#T(empty-tree) #T(tree) (empty))
    (#T(tree) #T(empty-tree) (empty))
    (#T(node :left l1 :value v1 :right r1) #T(tree)
     (destructuring-bind (l2 ans r2) (split v1 s2)
       (if ans
           (join (inter l1 l2) v1 (inter r1 r2))
         (concat (inter l1 l2) (inter r1 r2)) )
       ))
    ))

(defun diff (s1 s2)
  (um:match2 s1 s2
    (#T(empty-tree) #T(tree) (empty))
    (#T(tree) #T(empty-tree) (empty))
    (#T(node :left l1 :value v1 :right r1) #T(tree)
     (destructuring-bind (l2 ans r2) (split v1 s2)
       (if ans
           (concat (diff l1 l2) (diff r1 r2))
         (join (diff l1 l2) v1 (diff r1 r2)) )
       ))
    ))

(defun compare (s1 s2)
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
  (zerop (compare s1 s2)))

(defun subset (s1 s2)
  (um:match2 s1 s2
    (#T(empty-tree) #T(tree) t)
    (#T(tree) #T(empty-tree) nil)
    (#T(node :left l1 :value v1 :right r1)
       #T(node :left l2 :value v2 :right r2)
     (let ((c (ord:compare v1 v2)))
       (cond ((zerop c) (and (subset l1 l2)
                             (subset r1 r2)))
             ((minusp c) (and (subset (make-node
                                       :left l1
                                       :value v1)
                                      l2)
                              (subset r1 s2)))
             (t (and (subset (make-node
                              :value v1
                              :right r1)
                             r2)
                     (subset l1 s2)))
             )))
    ))


(defun iter (fn s)
  (um:match s
    (#T(empty-tree) nil)
    (#T(node :left l :value v :right r)
     (iter fn l)
     (funcall fn v)
     (iter fn r))
    ))

(defun fold (fn s accu)
  (um:match s
    (#T(empty-tree) accu)
    (#T(node :left l :value v :right r)
       (fold fn r (funcall fn v (fold fn l accu))))
    ))

(defun for-all (pred s)
  (um:match s
    (#T(empty-tree) t)
    (#T(node :left l :value v :right r)
       (and (funcall pred v)
            (for-all pred l)
            (for-all pred r)))
    ))

(defun exists (pred s)
  (um:match s
    (#T(empty-tree) nil)
    (#T(node :left l :value v :right r)
       (or (funcall pred v)
           (exists pred l)
           (exists pred r)))
    ))

(defun filter (pred s)
  (um:nlet filt ((accu (empty))
                 (s    s))
    (um:match s
      (#T(empty-tree) accu)
      (#T(node :left l :value v :right r)
         (filt (filt (if (funcall pred v)
                         (add v accu)
                       accu)
                     l)
               r))
      )))

(defun partition (pred s)
  (um:nlet part ((pair (list (empty) (empty)))
                 (s    s))
    (destructuring-bind (tp fp) pair
      (um:match s
        (#T(empty-tree)        pair)
        (#T(node :left l :value v :right r)
           (part (part (if (funcall pred v)
                           (list (add v tp) fp)
                         (list tp (add v fp)))
                       l)
                 r))
        ))))

(defun cardinal (s)
  (um:match s
    (#T(empty-tree) 0)
    (#T(node :left l :right r)
       (+ (cardinal l) 1 (cardinal r)))
    ))

(defun elements (s)
  (um:nlet iter ((accu nil)
                 (s    s))
    (um:match s
      (#T(empty-tree) accu)
      (#T(node :left l :value v :right r)
         (iter (cons v (iter accu r)) l))
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
(defun view-tree (s)
  (capi:contain
   (make-instance 'capi:graph-pane
                  :roots (list s)

                  :children-function (lambda (tree)
                                       (um:match tree
                                         (#T(empty-tree)  nil)
                                         (#T(node :left #T(empty-tree)
                                                  :right #T(empty-tree)) nil)
                                         (#T(node :left l
                                                  :right #T(empty-tree)) (list l))
                                         (#T(node :left #T(empty-tree)
                                                  :right r) (list r))
                                         (#T(node :left l
                                                  :right r) (list l r))))
                  
                  :print-function (lambda (node)
                                    (um:match node
                                      (#T(empty-tree)        "")
                                      (#T(node :value v) (format nil "~D" v))))
                  )))

#|
;; examine effects of constructing a tree in pure ascending or descending order
(view-tree (let ((xt (empty))) (dotimes (ix 100) (setf xt (add ix xt))) xt))
(view-tree (let ((xt (empty))) (dotimes (ix 100) (setf xt (add (- 100 ix) xt))) xt))
|#
;; -------------------------------------------------------------
#|
(defun tst (nmax)
  (um:nlet-tail iter ((n 0))
      (um:match n
        (k :when (>= k nmax) 'done)
        (n
         (when (= n (truncate nmax 2))
           (print 'halfway))
         (iter (1+ n)))
        )))
|#
