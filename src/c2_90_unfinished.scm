(import (rnrs)
        (base)
        (complex)
        (init)
        (functional)
        (generic-arithmetic)
        (symbolic-algebra)
        (coercion)
        (dict)
        (poly-terms)
        )

(init)

(define poly5 (make-polynomial 'y (make-termlist 'sparse (list (make-term 1
                                                                          (make-integer-number 1))
                                                               (make-term 0
                                                                          (make-integer-number 1))))))

(define poly6 (make-polynomial 'y (make-termlist 'sparse (list (make-term 2
                                                                          (make-integer-number 1))
                                                               (make-term 0
                                                                          (make-integer-number 1))))))

(define poly7 (make-polynomial 'y (make-termlist 'sparse (list (make-term 1
                                                                          (make-integer-number 1))
                                                               (make-term 0
                                                                          (make-integer-number -1))))))


(define poly8
  (make-polynomial 'x (make-termlist 'sparse (list (make-term 2 poly5)
                                                   (make-term 1 poly6)
                                                   (make-term 0 poly7)))))

(define poly9 (make-polynomial 'y (make-termlist 'sparse (list (make-term 1
                                                                          (make-integer-number 1))
                                                               (make-term 0
                                                                          (make-integer-number -2))))))
(define poly10 (make-polynomial 'y (make-termlist 'dense (list (make-term 3
                                                                           (make-integer-number 1))
                                                                (make-term 0
                                                                           (make-integer-number 7))))))

(define poly11 (make-polynomial 'x (make-termlist 'dense (list (make-term 1 poly9)
                                                                (make-term 0 poly10)))))

;(trace mul)
;(mul poly8 poly11)
;(beautiful-display (mul poly8 poly11))
