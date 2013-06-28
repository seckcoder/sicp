#lang racket

(define (pascal_triangle x y)
    (cond ((or (= y 0) (= x y)) 1)
          (else (+ (pascal_triangle (- x 1) (- y 1))
                   (pascal_triangle (- x 1) y)))))

(provide pascal_triangle)
