
(asdf:defsystem "data-objects"
  :description "data-objects: a collection of widely useful data types"
  :version     "1.0"
  :author      "D.McClain <dbm@spectrodynamics.com>"
  :license     "Copyright (c) 2008 by SpectroDynamics, LLC. All rights reserved."
  :components  ((:file "packages")
                ;; (:file "data-objects")
                (:file "reppy-channels")
                #+:LISPWORKS (:file "progress-bar")
                #+:LISPWORKS (:file "debug-stream")
                #+:LISPWORKS (:file "topgui")
                ;;(:file "sets")
                (:file "collector")
                (:file "ord")
                (:file "rb-trees")
                (:file "typed-rb-trees")
                (:file "typed-rb-tree-maps")
                (:file "queues")
                (:file "stacks")
                ;; (:file "single-reader-mailbox")
                ;; (:file "biqueue")
                ;; (:file "multiple-reader-mailbox")

                ;; (:file "protocols")
		;; (:file "btree")
                ;; (:file "memory-btrees")

                (:file "btree-clos")
                (:file "memory-btrees-clos")
                
                (:file "priority-queue")
                (:file "rpsx")
                ;; #+:LISPWORKS6 (:file "lw6-stm")
                (:file "simple-vstm")
                )
  :serial t
  :depends-on   ("useful-macros"
                 "mpcompat"
                 ;; "stm"
                 ))

