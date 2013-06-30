#lang racket

(define (newtons-method f guess)
  (fixed-point-of-transform f newton-transform guess))

(define (fixed-point-of-transform g transform guess)
  (fixed-point (transform g) guess))

(define (fixed-point f guess)
  (let ((new-guess (f guess)))
    (if (good-enough? guess new-guess)
      new-guess
      (fixed-point f new-guess))))


;; x = x - f(x) / f'(x)
(define (newton-transform f)
  (lambda (x) (- x
                 (/ (f x)
                    (derivative f x)))))

(define (derivative f x)
  (define dx 0.00001)
  (/ (- (f (+ x dx))
        (f x))
     dx))

(define (good-enough? old-guess new-guess)
  (> 0.001
     (abs (/ (- new-guess old-guess)
             new-guess))))

(provide newtons-method fixed-point-of-transform fixed-point)
