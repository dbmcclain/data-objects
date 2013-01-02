;; packages.lisp
;; DM/RAL  02/09
;; ------------------------------------------------------------------

(defpackage #:reppy-channels
  (:use #:common-lisp)
  (:nicknames #:rch)
  (:export
   #:channel
   #:spawn
   #:make-channel
   #:recvevt
   #:sendevt
   #:alwaysevt
   #:wrap
   #:wrap-abort
   #:guard
   #:choose
   #:sync
   #:poll
   #:select
   #:send
   #:recv
   #:discard-channel
   #:with-nack))

(defpackage #:topgui
  (:use #:common-lisp)
  (:export
   #:define-toplevel-app-interface
   #:run-toplevel-app-interface))

(defpackage #:data-objects
  (:use #:common-lisp)
  (:nicknames #:dobj)
  (:export
   #:get-item
   #:put-item
   #:data-available-p
   #:basic-data-object
   #:basic-fifo-queue
   #:basic-lifo-stack
   #:mp-shared-mixin
   #:mp-shared-data-object
   #:mp-shared-fifo-queue
   #:mp-shared-lifo-stack
   ;; #:with-lock
   #:with-locked-access
   #:queue-data
   #:stack-data

   ;; the following needed to overcome bug in hqn 4.1x
   #:my-process-wait
   #:my-process-wait-with-timeout
   #:my-process-lock
   #:my-mailbox-read
   #:my-with-lock))

(defpackage :queue
  (:use #:common-lisp)
  (:shadow
   #:push
   #:pop
   #:length
   #:delete
   #:delete-if
   #:find
   #:find-if
   #:map
   #:every
   #:some
   #:position
   #:position-if
   #:nth
   #:count
   #:count-if
   #:reduce
   #:member
   )
  (:export
   #:queue
   #:create
   #:clear
   #:add
   #:push
   #:peek
   #:top
   #:take
   #:pop
   #:copy
   #:is-empty
   #:not-empty
   #:length
   #:map
   #:iter
   #:fold
   #:transfer
   #:contents
   #:tail
   #:delete
   #:delete-if
   #:find
   #:find-if
   #:every
   #:some
   #:list-of
   #:position
   #:position-if
   #:count
   #:count-if
   #:nth
   #:reduce
   #:member
   #:do-queue
   ))

(defpackage :stack-on-list
  (:use #:common-lisp)
  (:shadow #:push #:pop)
  (:nicknames #:stackl)
  (:export
   #:stack
   #:create
   #:clear
   #:copy
   #:push
   #:top
   #:pop
   #:is-empty
   #:depth
   #:iter
   ))

(defpackage :stack-on-vector
  (:use #:common-lisp)
  (:shadow #:push #:pop)
  (:nicknames #:stackv)
  (:export
   #:stack
   #:create
   #:clear
   #:copy
   #:push
   #:top
   #:pop
   #:is-empty
   #:depth
   #:iter
   ))

(defpackage :single-reader-mailbox
  (:use #:common-lisp)
  (:nicknames #:srmb)
  (:import-from #:queue
   #:peek
   #:is-empty
   #:not-empty)
  (:export
   #:mailbox
   #:create
   #:send
   #:receive
   #:peek
   #:is-empty
   #:not-empty
   #:selective-receive
   ))

(defpackage :com.ral.biqueue
  (:use #:COMMON-LISP)
  (:nicknames #:BIQUEUE)
  (:export
   #:biqueue
   #:enqueue-fore
   #:enqueue-aft
   #:dequeue
   ))

(defpackage :multiple-reader-mailbox
  (:use #:common-lisp)
  (:nicknames #:mrmb)
  (:import-from #:queue
   #:peek
   #:is-empty
   #:not-empty)
  (:export
   #:mailbox
   #:create
   #:send
   #:receive
   #:peek
   #:is-empty
   #:not-empty
   ))

(defpackage #:btree
  (:use #:common-lisp)
  (:export
   #:node
   #:btree
   
   #:btree-protocol
   #:make-btree

   #:items-count
   #:root-node
   #:compare-fn
   #:key-fn
   #:make-node
   #:discard-node

   #:node-height
   #:node-fill-pointer
   #:node-list-cell
   #:node-capacity
   #:copy-node-list-cells
   #:coerce-to-object
   
   #:first-item
   #:last-item
   #:map-tree
   #:find-item
   #:insert-item
   #:add/update-item
   #:delete-item

   #:create-cursor
   #:cursor-next
   #:cursor-previous

   #:check-cache
   #:update-cache
   #:clear-cache
   #:cache-id

   #:btree-lock
   #:with-locked-btree
   #:get-cache
   ))

(defpackage :memory-btrees
  (:use #:common-lisp)
  (:export
   #:make-btree
   ))

(defpackage #:protocol
  (:use #:common-lisp)
  (:export
   #:define-protocol
   #:implements-protocol
   ))

(defpackage :rps
  (:use #:common-lisp)
  (:shadow #:signal)
  (:export
   #:with-noticed-mutations
   #:make-noticed-mutable-object
   #:make-ephemeral-cell
   #:value
   #:*environment*
   #:make-environment
   #:add-dependent
   #:remove-dependent
   #:clear-dependents
   #:clear-all-dependents
   #:add-observer
   #:remove-observer
   #:clear-observers
   #:clear-all-observers
   #:register-notification-action
   #:remove-notification-action
   #:clear-notification-actions
   #:notify
   #:enqueue-action
   #:enqueue-after-action
   #:enqueue
   #:enqueue-after
   #:define-monitored-class
   #:noticed-slots-metalevel-class
   #:noticed-slots-root-class
   ))

(defpackage :ord
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

(defpackage :sets
  (:use #:common-lisp)
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

(defpackage :maps
  (:use #:common-lisp)
  (:shadow #:find #:equal #:map #:remove)
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


(defpackage :com.ral.priority-queue
  (:use #:common-lisp)
  (:nicknames #:prioq)
  (:export
   #:priority-queue
   #:add-item
   #:remove-item
   ))


(defpackage :sets-internal
  (:use #:common-lisp)
  (:shadow #:merge)
  (:export
   #:create
   #:bal
   #:join
   #:not-found
   #:invalid-argument
   #:merge
   #:concat
   #:cons-enum
   ))

(defpackage :rb-tree
  (:use #:common-lisp)
  (:shadow #:merge #:equal #:remove #:union)
  (:export
   #:node
   #:node-left
   #:node-right
   #:node-height
   #:node-value
   #:empty-tree
   #:tree
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

   #:cons-enum
   ))

(defpackage :rb-tree-maps
  (:use #:common-lisp)
  (:shadow #:find #:equal #:map #:remove)
  (:import-from #:rb-tree
   #:cons-enum
   #:empty-tree
   #:node)
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

(defpackage #:lw6-stm
  (:use #:common-lisp)
  (:nicknames #:lwstm)
  (:shadow #:replace)
  (:export
   #:def-var
   #:def-accessor
   #:def-mutator

   #:with-mutation
   #:with-accessing
   ))

(defpackage #:debug-stream
  (:use #:common-lisp)
  (:nicknames #:dbgstrm #:dbgw)
  (:export
   #:make-debug-stream
   #:debug-print
   #:pr
   #:clear
   #:cls))

(defpackage #:progress-bar
  (:use #:common-lisp)
  (:nicknames #:pbar)
  (:export
   #:with-progress-bar
   #:incr-value
   #:set-value
   #:user-cancel))

(defpackage :simple-vstm
  (:use #:common-lisp)
  (:nicknames #:svstm)
  (:export
   #:var
   #:make-var
   #:var-val
   #:rmw
   ))

