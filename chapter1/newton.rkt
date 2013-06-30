#lang racket

;; x = x - f(x) / f'(x)
(define (newtons-method f guess)
  (fixed-point (newton-transform f)
               guess))

(define (newton-transform f)
  (lambda (x) (- x
                 (/ (f x)
                    (derivative f x)))))
(define (derivative f x)
  (define dx 0.001)
  (/ (- (f (+ x dx))
        (f x))
     dx))

(define (good-enough? old-guess new-guess)
  (> 0.01
     (/ (abs (- new-guess old-guess))
        new-guess)))

(define (fixed-point f guess)
  (let ([new-guess (f guess)])
    (if (good-enough? guess new-guess)
      new-guess
      (fixed-point f new-guess))))

(provide newtons-method fixed-point)
