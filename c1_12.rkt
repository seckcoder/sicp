(define (pascal_traiangle x y)
    (cond ((or (= y 0) (= (+ 1 x) y)) 1)
    (else (+ (pascal_traiangle (- x 1) (- y 1))
             (pascal_traiangle (- x 1) y)))))
