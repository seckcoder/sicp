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

(define poly5 (make-polynomial 'y (list (make-term 1
                                                   (make-integer-number 1))
                                        (make-term 0
                                                   (make-integer-number 1)))))

(define poly6 (make-polynomial 'y (list (make-term 2
                                                   (make-integer-number 1))
                                        (make-term 0
                                                   (make-integer-number 1)))))

(define poly7 (make-polynomial 'y (list (make-term 1
                                                   (make-integer-number 1))
                                        (make-term 0
                                                   (make-integer-number -1)))))

; [(y+1)x^2+(y^2+1)x+(y-1)]
(define poly8
  (make-polynomial 'x (list (make-term 2 poly5)
                            (make-term 1 poly6)
                            (make-term 0 poly7))))

(define poly9 (make-polynomial 'y (list (make-term 1
                                                   (make-integer-number 1))
                                        (make-term 0
                                                   (make-integer-number -2)))))

(define poly10 (make-polynomial 'y (list (make-term 3
                                                    (make-integer-number 1))
                                         (make-term 0
                                                    (make-integer-number 7)))))

; [(y-2)x+(y^3+7)]
(define poly11 (make-polynomial 'x (list (make-term 1 poly9)
                                         (make-term 0 poly10))))

;(display poly8)(newline)
;(display poly11)(newline)
;(newline)
(display (add poly8 poly11))

;(display (add (make-polynomial 'x (list (make-term 0 (make-integer-number 2))))
              ;(make-polynomial 'x (list (make-term 0 (make-integer-number 4))))))
