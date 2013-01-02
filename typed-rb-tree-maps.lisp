
;; --------------------------------------------
(in-package :rb-tree-maps)
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
  (rb-tree:empty))

(defun is-empty (map)
  (rb-tree:is-empty map))

(defun mem (x map)
  (rb-tree:mem x map))

(defun remove (x map)
  (rb-tree:remove x map))

(defun add (key val map)
  (rb-tree:add (make-map-cell
                :key key
                :val val)
               map))

(defun find (key map &optional default)
  (um:match map
    (#T(empty-tree)   (values default nil))
    (#T(node :left l :value v :right r)
       (let ((c (ord:compare key (map-cell-key v))))
         (cond ((zerop c) (values (map-cell-val v) t))
               (t         (find key (if (minusp c) l r)))
               )))
    ))

(defun compare (cmp map1 map2)
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
  (zerop (compare cmp map1 map2)))

(defun fold (f map accu)
  (um:match map
    (#T(empty-tree)  accu)
    (#T(node :left l :value v :right r)
       (fold f r
             (funcall f
                      (map-cell-key v)
                      (map-cell-val v)
                      (fold f l accu))) )
    ))

(defun map (f map)
  (um:match map
    (#T(empty-tree) nil)
    (#T(node :left l :value v :right r :height h)
       (list
        (map f l)
        (make-map-cell
         :key (map-cell-key v)
         :val (funcall f (map-cell-val v)))
        (map f r)
        h))
    ))

(defun mapi (f map)
  (um:match map
    (#T(empty-tree) nil)
    (#T(node :left l :value v :right r :height h)
       (list
        (mapi f l)
        (let ((key (map-cell-key v)))
          (make-map-cell
           :key key
           :val (funcall f key (map-cell-val v))))
        (mapi f r)
        h))
    ))

(defun iter (f map)
  (um:match map
    (#T(empty-tree)  nil)
    (#T(node :left l :value v :right r)
     (iter f l)
     (funcall f (map-cell-key v) (map-cell-val v))
     (iter f r))
    ))

