; This problem is solved in branch c2_81-82
; a)
; there will be infinite loop. since apply-generic will call apply-generic
; again(with complex number coerced to complex number)
; when it cannot find the exp proc for (complex-number complex-number).
; b)
; ...
; c)
; in lib/base.scm
