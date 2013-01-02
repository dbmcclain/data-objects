;; sets.lisp
;; --------------------------------------------------------------------------------------
;; Binary tree storage for unique ordered items, plus Maps, FIFO Queues, and Stacks
;;
;; Copyright (C) 2008 by SpectroDynamics, LLC. All rights reserved.
;;
;; DM/SD  08/08
;; --------------------------------------------------------------------------------------

;; ------------------------------------------------------------------------
(in-package :ord)
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

(defstruct ci-char
  c)

(defmethod compare ((a ci-char) (b ci-char))
  (cond ((char-equal a b) 0)
        ((char-lessp a b) -1)
        (t 1)))

(defstruct ci-string
  s)

(defmethod compare ((a ci-string) (b ci-string))
  (cond ((string-equal a b) 0)
        ((string-lessp a b) -1)
        (t 1)))

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
       (every #'= (array-dimensions a) (array-dimensions b))
       (eql (adjustable-array-p a) (adjustable-array-p b))
       (eql (array-element-type a) (array-element-type b))
       (every #'compare=
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
       (every #'compare= a b)))
|#
;; ------------------------------------------------------------------------

;; --------------------------------------------

;; ------------------------------------------------------------------------
(in-package :sets-internal)
;; ------------------------------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)
  (import '(sets:height
            sets:add
            sets:compare
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

(defstruct node
  left val right height)

;; ----------------------------------------------------------------
#|
(defmacro lr (((l r) node) &body body)
  `(with-accessors ((,l node-left )
                    (,r node-right)) ,node
     ,@body))

(defmacro lvr (((l v r) node) &body body)
  `(with-accessors ((,l node-left )
                    (,v node-val  )
                    (,r node-right)) ,node
     ,@body))

(defmacro lvrh (((l v r h) node) &body body)
  `(with-accessors ((,l node-left  )
                    (,v node-val   )
                    (,r node-right )
                    (,h node-height)) ,node
     ,@body))
|#

#|
(defmacro lr (((l r) node) &body body)
  (let ((gnode (gensym)))
    `(let ((,gnode ,node))
       (multiple-value-bind (,l ,r)
           (values (node-left  ,gnode)
                   (node-right ,gnode))
         ,@body))
    ))

(defmacro lvr (((l v r) node) &body body)
  (let ((gnode (gensym)))
    `(let ((,gnode ,node))
       (multiple-value-bind (,l ,v ,r)
           (values (node-left  ,gnode)
                   (node-val   ,gnode)
                   (node-right ,gnode))
         ,@body))
    ))

(defmacro lvrh (((l v r h) node) &body body)
  (let ((gnode (gensym)))
    `(let ((,gnode ,node))
       (multiple-value-bind (,l ,v ,r ,h)
           (values (node-left   ,gnode)
                   (node-val    ,gnode)
                   (node-right  ,gnode)
                   (node-height ,gnode))
         ,@body))
    ))
|#

#| |#
(defmacro lr (((l r) node) &body body)
  (let ((gnode (gensym)))
    `(um:bind*
         ((,gnode ,node)
          (:values (,l ,r) (values (node-left  ,gnode)
                                   (node-right ,gnode))))
       ,@body)))

(defmacro lvr (((l v r) node) &body body)
  (let ((gnode (gensym)))
    `(um:bind*
         ((,gnode ,node)
          (:values (,l ,v ,r) (values (node-left  ,gnode)
                                      (node-val   ,gnode)
                                      (node-right ,gnode))))
       ,@body)))

(defmacro lvrh (((l v r h) node) &body body)
  (let ((gnode (gensym)))
    `(um:bind*
         ((,gnode ,node)
          (:values (,l ,v ,r ,h) (values (node-left   ,gnode)
                                         (node-val    ,gnode)
                                         (node-right  ,gnode)
                                         (node-height ,gnode))))
       ,@body)))
#| |#

#|
(defmacro lr (((l r) node) &body body)
  (let ((pat (um::make-row-type
              :type  'node
              :slots '(left right)
              :pats  (list l r))))
    `(um:match ,node
       (,pat ,@body))
    ))

(defmacro lvr (((l v r) node) &body body)
  (let ((pat (um::make-row-type
              :type  'node
              :slots '(left val right)
              :pats  (list l v r))))
    `(um:match ,node
       (,pat ,@body))
    ))

(defmacro lvrh (((l v r h) node) &body body)
  (let ((pat (um::make-row-type
              :type  'node
              :slots '(left val right height)
              :pats  (list l v r h))))
    `(um:match ,node
       (,pat ,@body))
    ))
|#


     
#+:LISPWORKS
(editor:setup-indent "lr" 2 2)
#+:LISPWORKS
(editor:setup-indent "lvr" 2 2)
#+:LISPWORKS
(editor:setup-indent "lvrh" 2 2)

;; ----------------------------------------------------------------

;; create - create a tree node with left son l, value v, and right son r.
;; We must have all elements of l < v < all elements of r.
;; l and r must be balanced and have a height difference <= 2

(defun create (l v r)
  (make-node
   :left   l
   :val    v
   :right  r
   :height (1+ (max (height l) (height r)))
   ))


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
             (unless l
               (invalid-arg))
             (lvr ((ll lv lr) l)
                 (if (>= (height ll) (height lr))
                     (create ll lv (create lr v r))
                   (progn
                     (unless lr
                       (invalid-arg))
                     (lvr ((lrl lrv lrr) lr)
                         (create (create ll lv lrl) lrv (create lrr v r))
                       )))))

            ((> hr (+ 2 hl))
             (unless r
               (invalid-arg))
             (lvr ((rl rv rr) r)
                 (if (>= (height rr) (height rl))
                     (create (create l v rl) rv rr)
                   (progn
                     (unless rl
                       (invalid-arg))
                     (lvr ((rll rlv rlr) rl)
                         (create (create l v rll) rlv (create rlr rv rr))
                       )))))

            (t (make-node
                :left   l
                :val    v
                :right  r
                :height (1+ (max hl hr))))
            ))))
            
;; join -- same as create and bal, but no assumptions are made on the
;; relative heights of l and r

(defun join (l v r)
  (cond ((null l) (add v r))
        ((null r) (add v l))
        (t (lvrh ((ll lv lr lh) l)
               (lvrh ((rl rv rr rh) r)
                   (cond ((> lh (+ 2 rh)) (bal ll lv (join lr v r)))
                         ((> rh (+ 2 lh)) (bal (join l v rl) rv rr))
                         (t (create l v r))
                         ))))
        ))

(defun not-found ()
  (error "Not found"))

;; merge -- merge two trees l and r into one.
;; All elements of l must precede the elements of r
;; Assume height difference <= 2

(defun merge (t1 t2)
  (cond ((null t1) t2)
        ((null t2) t1)
        (t (bal t1 (min-elt t2) (remove-min-elt t2)))
        ))

;; concat - merge two trees l and r into one.
;; All elements of l must precede the elements of r.
;; No assumptions on the heights of l and r.

(defun concat (t1 t2)
  (cond ((null t1) t2)
        ((null t2) t1)
        (t (join t1 (min-elt t2) (remove-min-elt t2)))
        ))

(defun cons-enum (s e)
  (cond ((null s) e)
        (t (lvr ((l v r) s)
               (cons-enum l (list v r e))
             ))
        ))

(defun compare-aux (e1 e2)
  (cond ((and (null e1)
              (null e2)) 0)
        ((null e1) -1)
        ((null e2) 1)
        (t (destructuring-bind (v1 r1 e1) e1
             (destructuring-bind (v2 r2 e2) e2
               (let ((c (compare v1 v2)))
                 (if (zerop c)
                     (compare-aux (cons-enum r1 e1)
                                  (cons-enum r2 e2))
                   c)
                 ))))
        ))

(defun elements-aux (accu s)
  (cond ((null s) accu)
        (t (lvr ((l v r) s)
               (elements-aux (cons v (elements-aux accu r)) l)
             ))
        ))

;; ------------------------------------------------------------------------
(in-package :sets)
;; ------------------------------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)
  (import '(sets-internal:lr
            sets-internal:lvr
            sets-internal:lvrh
            sets-internal:node
            sets-internal:make-node
            sets-internal:node-left
            sets-internal:node-val
            sets-internal:node-right
            sets-internal:node-height
            sets-internal:bal
            sets-internal:join
            sets-internal:concat
            sets-internal:cons-enum
            sets-internal:compare-aux
            sets-internal:elements-aux
            sets-internal:not-found
            sets-internal:invalid-argument))

  (shadowing-import '(sets-internal:merge)))

;; --------------------------------------------

(defun height (node)
  (if node
      (node-height node)
    0))

;; add - insertion of one element
(defun add (x node)
  (if node
      (lvrh ((l v r h) node)
          (let ((c (compare x v)))
            (cond ((zerop c)  (make-node ;; to support maps (see below)
                                         :left   l
                                         :val    x  ;; cause new map value to be substituted for old value
                                         :right  r
                                         :height h))
                  ((minusp c) (bal (add x l) v r))
                  (t (bal l v (add x r)))
                  )))
    (singleton x)))

(defun min-elt (node)
  (cond ((null node) (not-found))
        ((null (node-left node)) (node-val node))
        (t (min-elt (node-left node)))
        ))

(defun max-elt (node)
  (cond ((null node) (not-found))
        ((null (node-right node)) (node-val node))
        (t (max-elt (node-right node)))
        ))

;; remove-min-elt - remove the smallest element of the set
;; also useful for priority-queues

(defun remove-min-elt (node)
  (cond ((null node) (invalid-argument "Sets-internal::remove-min-elt"))
        ((null (node-left node)) (node-right node))
        (t (lvr ((l v r) node)
               (bal (remove-min-elt l) v r)))
        ))

;; split - split x s returns a triple (l present r) where
;; - l is the set of elements of s that are < x
;; - r is the set of elements of s that are > x
;; - present is false if s contains no element equal to x
;;   or true if s contains an element equal to x

(defun split (x tree)
  (cond ((null tree) (list nil nil nil))
        (t (lvr ((l v r) tree)
               (let ((c (compare x v)))
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
        (t (lvr ((l v r) tree)
               (let ((c (compare x v)))
                 (or (zerop c)
                     (mem x (if (minusp c) l r)))
                 ))
           )))

(defun singleton (x)
  (make-node
   :val    x
   :height 1))

(defun remove (x tree)
  (cond ((null tree) nil)
        (t (lvr ((l v r) tree)
               (let ((c (compare x v)))
                 (cond ((zerop c) (merge l r))
                       ((minusp c) (bal (remove x l) v r))
                       (t (bal l v (remove x r)))
                       ))))
        ))

(defun union (s1 s2)
  (cond ((null s1) s2)
        ((null s2) s1)
        (t (lvrh ((l1 v1 r1 h1) s1)
               (lvrh ((l2 v2 r2 h2) s2)
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
                         ))))
        ))

(defun inter (s1 s2)
  (cond ((null s1) nil)
        ((null s2) nil)
        (t (lvr ((l1 v1 r1) s1)
               (destructuring-bind (l2 ans r2) (split v1 s2)
                 (if ans
                     (join (inter l1 l2) v1 (inter r1 r2))
                   (concat (inter l1 l2) (inter r1 r2)) )
                 )))
        ))


(defun diff (s1 s2)
  (cond ((null s1) nil)
        ((null s2) s1)
        (t (lvr ((l1 v1 r1) s1)
               (destructuring-bind (l2 ans r2) (split v1 s2)
                 (if ans
                     (concat (diff l1 l2) (diff r1 r2))
                   (join (diff l1 l2) v1 (diff r1 r2)) )
                 )))
        ))

(defmethod ord:compare ((s1 node) (s2 node))
  (compare-aux (cons-enum s1 nil)
               (cons-enum s2 nil)))

(defun equal (s1 s2)
  (zerop (compare s1 s2)))

(defun subset (s1 s2)
  (cond ((null s1) t)
        ((null s2) nil)
        (t (lvr ((l1 v1 r1) s1)
               (lvr ((l2 v2 r2) s2)
                   (let ((c (compare v1 v2)))
                     (cond ((zerop c) (and (subset l1 l2)
                                           (subset r1 r2)))
                           ((minusp c) (and (subset (make-node
                                                     :left   l1
                                                     :val    v1
                                                     :height 0)
                                                    l2)
                                            (subset r1 s2)))
                           (t (and (subset (make-node
                                            :val    v1
                                            :right  r1
                                            :height 0)
                                           r2)
                                   (subset l1 s2)))
                           )))))
        ))


(defun iter (fn s)
  (cond ((null s) nil)
        (t (lvr ((l v r) s)
               (iter fn l)
             (funcall fn v)
             (iter fn r)))
        ))

(defun fold (fn s accu)
  (cond ((null s) accu)
        (t (lvr ((l v r) s)
               (fold fn r (funcall fn v (fold fn l accu)))
             ))
        ))

(defun for-all (pred s)
  (cond ((null s) t)
        (t (lvr ((l v r) s)
               (and (funcall pred v)
                    (for-all pred l)
                    (for-all pred r))
             ))
        ))

(defun exists (pred s)
  (cond ((null s) nil)
        (t (lvr ((l v r) s)
               (or (funcall pred v)
                   (exists pred l)
                   (exists pred r))
             ))
        ))

(defun filter (pred s)
  (um:perform filt ((accu nil)
                    (s    s))
    (cond ((null s) accu)
          (t (lvr ((l v r) s)
                 (filt (filt (if (funcall pred v)
                                 (add v accu)
                               accu)
                             l)
                       r)))
          )))

(defun partition (pred s)
  (um:perform part ((pair (list nil nil))
                    (s    s))
    (destructuring-bind (tp fp) pair
      (cond ((null s) pair)
            (t (lvr ((l v r) s)
                   (part (part (if (funcall pred v)
                                   (list (add v tp) fp)
                                 (list tp (add v fp)))
                               l)
                         r)
                 ))
            ))))


(defun cardinal (s)
  (cond ((null s) 0)
        (t (lr ((l r) s)
               (+ (cardinal l) 1 (cardinal r))
             ))
        ))

(defun elements (s)
  (elements-aux nil s))

(defun choose (s)
  (min-elt s))


;; -------------------------------------------------------------
;; -------------------------------------------------------------

;; --------------------------------------------
(in-package :maps)
;; --------------------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)
  (import '(sets-internal:lvr
            sets-internal:lvrh
            sets-internal:make-node
            sets-internal:cons-enum)))

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
  (cond ((null map) (values default nil))
        (t (lvr ((l v r) map)
               (let ((c (ord:compare key (map-cell-key v))))
                 (cond ((zerop c) (values (map-cell-val v) t))
                       (t (find key (if (minusp c) l r)))
                       )) ))
        ))

(defun compare (cmp map1 map2)
  (um:perform compare-aux ((e1 (cons-enum map1 nil))
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
                                    (t (compare-aux (cons-enum r1 e1)
                                                    (cons-enum r2 e2)))
                                    ))))))))
          )))

(defun equal (cmp map1 map2)
  (zerop (compare cmp map1 map2)))


(defun fold (f map accu)
  (cond ((null map) accu)
        (t (lvr ((l v r) map)
               (fold f r (funcall f (map-cell-key v) (map-cell-val v) (fold f l accu))) ))
        ))

(defun map (f map)
  (cond ((null map) nil)
        (t (lvrh ((l v r h) map)
               (make-node
                :left (map f l)
                :val  (make-map-cell
                       :key (map-cell-key v)
                       :val (funcall f (map-cell-val v)))
                :right (map f r)
                :height h)))
        ))

(defun mapi (f map)
  (cond ((null map) nil)
        (t (lvrh ((l v r h) map)
               (make-node
                :left (mapi f l)
                :val  (let ((key (map-cell-key v)))
                        (make-map-cell
                         :key key
                         :val (funcall f key (map-cell-val v))))
                :right (mapi f r)
                :height h)))
        ))

(defun iter (f map)
  (cond ((null map) nil)
        (t (lvr ((l v r) map)
               (iter f l)
             (funcall f (map-cell-key v) (map-cell-val v))
             (iter f r)))
        ))

;; -------------------------------------------------------------------
;; -------------------------------------------------------------

;; -------------------------------------------------------------------
(in-package :queue)
;; -------------------------------------------------------------------
;; FIFO queues with in-place modification
;;
;; A queue is a reference to either nothing or some cell of a cyclic list.
;; By convention, that cell is to be viewed as the last cell in the queue.
;; The first cell in the queue is then found in constant time: it is the next
;; cell in the cyclic list. The queue's length is also recorded, so as to make
;; (length) a constant-time operation.

(defstruct queue
  (length 0)
  (tail   nil))

(defun create ()
  (make-queue))

(defun clear (q)
  (setf (queue-length q) 0
        (queue-tail q)   nil))

(defun add (x q)
  (incf (queue-length q))
  (if (= 1 (queue-length q))
      (let ((cell (cons x nil)))
        (setf (cdr cell) cell
              (queue-tail q) cell))
    ;; else...
    (let* ((tail (queue-tail q))
           (head (cdr tail))
           (cell (cons x head)))
      (setf (cdr tail) cell
            (queue-tail q) cell)))
  x)

(defun push (x q)
  (add x q))

(defun peek (q &optional empty-value)
  (if (zerop (queue-length q))
      (values empty-value nil)
    (values (cadr (queue-tail q)) t)))

(defun top (q &optional empty-value)
  (peek q empty-value))

(defun tail (q)
  (queue-tail q))

(defun take (q &optional (empty-error-p t) empty-value)
  (if (zerop (queue-length q))
      (if empty-error-p
          (error "Queue empty")
        (values empty-value nil))
    (let* ((tail (queue-tail q))
           (head (cdr tail)))
      (if (eq head tail)
          (setf (queue-tail q) nil)
        (setf (cdr tail) (cdr head)))
      (decf (queue-length q))
      (values (car head) t))))

(defun pop (q &optional (empty-error-p t) empty-value)
  (take q empty-error-p empty-value))

(defun remove-at (q cursor)
  ;; internal routine shared with selective mailbox receive routine
  ;; assumes cursor is one of the spine elements of the queue
  ;; cursor points to the cons cell immediately preceding the value to be removed.
  (if (eq (cdr cursor) cursor)
      (setf (queue-tail q) nil) ;; remove one and only queue element
    (progn
      (if (eq (queue-tail q) (cdr cursor))
          (setf (queue-tail q) cursor))   ;; adjust queue tail if removing last element
      (setf (cdr cursor) (cddr cursor)))) ;; remove element at (cdr cursor)
  (decf (queue-length q)))

(defun copy (q)
  (if (zerop (queue-length q))
      (create)
    (let* ((tail     (queue-tail q))
           (new-tail (cons (car tail) nil)))
      (setf (cdr new-tail) new-tail)
      (setf (cdr new-tail) (um:perform iter ((cell (cdr tail)))
                             (if (eq cell tail)
                                 new-tail
                               (let ((new-cell (cons (car cell) nil)))
                                 (setf (cdr new-cell) (iter (cdr cell)))
                                 new-cell))))
      (make-queue
       :length (queue-length q)
       :tail   new-tail)
      )))

(defun not-empty (q)
  (queue-tail q))

(defun is-empty (q)
  (zerop (queue-length q)))

(defun length (q)
  (queue-length q))

(defun iter (f q)
  (if (plusp (queue-length q))
      (let ((tail (queue-tail q)))
        (um:perform inner-iter ((cell (cdr tail)))
          (funcall f (car cell))
          (unless (eq cell tail)
            (inner-iter (cdr cell)))
          ))
    ))

(defun fold (f accu q)
  (if (zerop (queue-length q))
      accu
    (let ((tail (queue-tail q)))
      (um:perform inner-fold ((accu accu)
                              (cell (cdr tail)))
        (let ((new-accu (funcall f accu (car cell))))
          (if (eq cell tail)
              new-accu
            (inner-fold new-accu (cdr cell)) )))
      )))

(defun transfer (q1 q2)
  (let ((len1 (queue-length q1)))
    (if (plusp len1)
        (let ((tail1 (queue-tail q1)))
          (clear q1)
          (if (plusp (queue-length q2))
              (let* ((tail2 (queue-tail q2))
                     (head1 (cdr tail1))
                     (head2 (cdr tail2)))
                (setf (cdr tail1) head2
                      (cdr tail2) head1)))
          (incf (queue-length q2) len1)
          (setf (queue-tail q2) tail1)
          ))))

(defun contents (q)
  ;; destructively read the queue
  ;; returning its contents as a list
  ;; the queue structure is then reset to empty
  (um:when-let (tl (queue-tail q))
    (let ((lst (cdr tl)))
      (setf (queue-tail q)   nil
            (queue-length q) 0
            (cdr tl)         nil)
      lst)))

;; -------------------------------------------------------------------
;; -------------------------------------------------------------

;; -------------------------------------------------------------------
(in-package :stack-on-list)
;; -------------------------------------------------------------------

(defstruct stack
  (depth 0)
  (stk nil))

(defun create ()
  (make-stack))

(defun clear (s)
  (setf (stack-stk s) nil
        (stack-depth s) 0))

(defun copy (s)
  (make-stack
   :stk   (stack-stk s)
   :depth (stack-depth s)))

(defun push (x s)
  (cl:push x (stack-stk s))
  (incf (stack-depth s)))

(defun check-stack (s fn empty-error-p empty-value)
  (if (null (stack-stk s))
      (if empty-error-p
          (error "Stack empty")
        (values empty-value nil))
    (values (funcall fn) t)))

(defun pop (s &optional (empty-error-p t) empty-value)
  (check-stack s #'(lambda ()
                     (decf (stack-depth s))
                     (cl:pop (stack-stk s)))
               empty-error-p empty-value))

(defun top (s &optional (empty-error-p t) empty-value)
  (check-stack s #'(lambda ()
                     (first (stack-stk s)))
               empty-error-p empty-value))

(defun is-empty (s)
  (null (stack-stk s)))

(defun depth (s)
  (stack-depth s))

(defun iter (f s)
  (dolist (item (stack-stk s))
    (funcall f item)))


;; -------------------------------------------------------------------
;; -------------------------------------------------------------

;; -------------------------------------------------------------------
(in-package :stack-on-vector)
;; -------------------------------------------------------------------

(defstruct stack
  (stk (make-array 0
                   :adjustable t
                   :fill-pointer t)))

(defun create ()
  (make-stack))

(defun clear (s)
  (setf (fill-pointer (stack-stk s)) 0))

(defun copy (s)
  (make-stack
   :stk (stack-stk s)))

(defun push (x s)
  (vector-push-extend x (stack-stk s)))

(defun check-stack (s fn empty-error-p empty-value)
  (if (zerop (fill-pointer (stack-stk s)))
      (if empty-error-p
          (error "Stack empty")
        (values empty-value nil))
    (values (funcall fn) t)))

(defun pop (s &optional (empty-error-p t) empty-value)
  (check-stack s #'(lambda ()
                     (vector-pop (stack-stk s)))
               empty-error-p empty-value))

(defun top (s &optional (empty-error-p t) empty-value)
  (check-stack s #'(lambda ()
                     (let ((stk (stack-stk s)))
                       (aref stk (1- (fill-pointer stk)))))
               empty-error-p empty-value))

(defun is-empty (s)
  (zerop (fill-pointer (stack-stk s))))

(defun depth (s)
  (fill-pointer (stack-stk s)))

(defun iter (f s)
  (let ((stk (stack-stk s)))
    (dotimes (ix (fill-pointer stk))
      (funcall f (aref stk ix))) ))


