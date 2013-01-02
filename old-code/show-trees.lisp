
#|
(defun make-tree (&optional (tree (sets:empty)))
  (if (= (sets:height tree) 10)
      tree
    (make-tree (sets:add (random 16384) tree))))

(defun tree-to-list (tree)
  (if (null tree)
      nil
    (list (tree-to-list (sets-internal:node-left tree))
          (sets-internal:node-val tree)
          (tree-to-list (sets-internal:node-right tree)))
    ))

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

(defun view-set (s)
  (capi:contain
   (make-instance 'capi:graph-pane
                  :roots (list s)

                  :children-function (lambda (tree)
                                       (cond ((and (null (sets-internal:node-left tree))
                                                   (null (sets-internal:node-right tree)))
                                              nil)

                                             (t (list (or (sets-internal:node-left tree)
                                                          (sets:singleton ""))
                                                      (or (sets-internal:node-right tree)
                                                          (sets:singleton ""))
                                                      ))
                                             ))
                  
                  :print-function (lambda (node)
                                    (format nil "~D" (sets-internal:node-val node)))
                  )))

(defun view-hash-keys (bucket-index)
  (damf::get-bucket bucket-index damf::*current-hash-db*)
  (let ((v (damf::get-bucket-key-positions
            (damf::hash-file-info-bucket damf::*current-hash-db*)))
        (xt (sets:empty)))
    (dotimes (ix (length v))
      (setf xt (sets:add (aref v ix) xt)))
    (view-set xt)))

#|
;; examine effects of constructing a tree in pure ascending or descending order
(view-set (let ((xt (sets:empty))) (dotimes (ix 100) (setf xt (sets:add ix xt))) xt))
(view-set (let ((xt (sets:empty))) (dotimes (ix 100) (setf xt (sets:add (- 100 ix) xt))) xt))
|#