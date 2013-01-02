
(protocol:define-protocol btree-protocol (btree node)
  (:type
   (height         integer)
   (index          integer)
   (count          integer)
   (node-or-nil    (or node (eql nil)))
   (node-or-object (or node t))
   (undefined      t))
  
  (:signature
   ( node-height (node) => height
     :documentation "(node-height node)
Return the height of the node.")
   ( node-fill-pointer (node) => index
     :documentation "(node-fill-pointer node)
Return the current fill pointer of the node.")
   ( (setf node-fill-pointer) (index node) => index
     :documentation "(setf (node-fill-pointer node) index)
Set the current fill pointer of the node to index.")
   ( node-list-cell (node index) => node-or-object
     :documentation "(node-list-cell node index)
Return the contents of the node cell at index.")
   ( (setf node-list-cell) (node-or-object node index) => node-or-object
     :documentation "(setf (node-list-cell node index) node-or-object)
Set the contents of the node cell at index.")
   ( node-capacity (node) => count
     :documentation "(node-capacity node)
Return the maximum number of usable cells in the node.
This should not include the extra 2 cells at the end used for intermediate results.")
   ( copy-node-list-cells (node index node index count) => undefined
     :documentation "(copy-node-list-cells to-node to-index from-node from-index ncells)
Copy ncells from the from-node starting at from-index,
to the to-node starting at to-index.")

   ( root-node (btree) => node-or-nil
     :documentation "(root-node btree)
Return the root node of the tree.")
   ( (setf root-node) (node-or-nil btree) => node-or-nil
     :documentation "(setf (root-node btree) node)
Set the root node of the tree.")
   ( items-count (btree) => count
     :documentation "(items-count btree)
Return the number of items in the tree.")
   ( (setf items-count) (count btree) => count
     :documentation "(setf (items-count btree) count)
Set the number of items in the tree.")
   ( compare-fn (btree) => function
     :documentation "(compare-fn btree)
Return the function used by the tree for comparing keys.")
   ( key-fn (btree) => function
     :documentation "(key-fn btree)
Return the function used by the tree for comparing keys.")
   ( make-node (btree height) => node
     :documentation "(make-node btree height)
Allocate a new tree node with the given height and return the node.")
   ( discard-node (btree node) => undefined
     :documentation "(discard-node btree node)
Discard the node, doing whatever may be necessary to recycle storage.")
   ))

;; --------------------------------------
;; define the implementing methods here, e.g., for class file-btree and file-btree-node
;; --------------------------------------


;; --------------------------------------
;; This now checks that there are methods using the new concrete classes that
;; cover the entire protocol signature.

(protocol:implements-protocol btree-protocol (file-btree file-btree-node))

;; --------------------------------------
;; --------------------------------------
