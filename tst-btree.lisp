
(in-package :btree)

(setf tree (memory-btrees:make-btree :node-size 15))
(loop for ix from 1 to 100 do (insert-item tree ix ix))
(inspect tree)
(view-btree tree)
(delete-item tree 30)
(delete-item tree 31)
(loop for ix from 32 to 36 do (delete-item tree ix :delete-fn #'print))
(delete-item tree 37)
(loop for ix from 38 to 50 do (delete-item tree ix :delete-fn #'print))
(map-tree tree #'print)
(loop for ix from 5 to 95 do (delete-item tree ix :delete-fn #'print))
