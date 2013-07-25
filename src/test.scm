(import (rnrs)
        (base)
        (complex)
        (init)
        (functional)
        (generic-arithmetic)
        (symbolic-algebra)
        (dict)
        )

(init)
; x^2 + x
(define poly1 (make-polynomial 'x (list (make-term 2
                                                   (make-integer-number 1))
                                        (make-term 1
                                                   (make-integer-number 1)))))
; x + 1
(define poly2 (make-polynomial 'x (list (make-term 1
                                                   (make-integer-number 1))
                                        (make-term 0
                                                   (make-integer-number 1)))))

; [3x^2+(2+3i)x+7]
(define poly3
  (make-polynomial 'x
                   (list (make-term 2
                                    (make-integer-number 3))
                         (make-term 1
                                    (make-complex-from-real-imag 2 3))
                         (make-term 0
                                    (make-integer-number 7)))))
; [x^4+(2/3)x^2+(5+3i)]
(define poly4
  (make-polynomial 'x
                   (list (make-term 4
                                    (make-integer-number 1))
                         (make-term 2
                                    (make-rational-number 2 3))
                         (make-term 0
                                    (make-complex-from-real-imag 5 3)))))

;(display poly3)(newline)
;(display poly4)(newline)
(display (mul poly3 poly4))
