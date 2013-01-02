
(DEFPACKAGE :com.ral.sugar-qi
  (:USE :CL :CL-USER :qi)
  (:EXPORT
    :cases
    :match
    :db
    :second
    :third
     ))

(IN-PACKAGE :com.ral.sugar-qi)

(load "sugar.qi")

(IMPORT [cases match db] :CL-USER)

