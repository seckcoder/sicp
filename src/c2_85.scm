; the drop procedure is in lib/base.scm(named dropif)
; There is some problem with the question.
; The question asks to rewrite apply-generic using drop, so that we can
; simplifiy the answer when we an. However,
; the return value of apply-generic may not be typed arithmetic value in tower.
; For example, the equ? should return bool, which cannot droped. In real
; world application(like the arithmetic system implemented in chez-scheme), I
; don't think this problem exists since everyting is typed. However, for this
; question, we have to implement a tricky procedure named typed?(in lib/base.scm)
; to determine whether we should drop it.
;
(import (rnrs)
        (base)
        (complex)
        (init)
        (functional)
        (generic-arithmetic)
        (dict)
        )

(init)
(define real (make-real-number 3.5))
(display (add real real))(newline) ; this will print (integer . 7)
