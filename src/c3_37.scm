(import (constraint))

(define (c+ x y)
  (let ((z (make-connector)))
    (make-adder x y z)
    z))

(define (c* x y)
  (let ((z (make-connector)))
    (make-multiplier x y z)
    z))

(define (cv constant)
  (let ((c (make-connector)))
    (make-constant constant c)
    c))

(define (c/ x y)
  (let ((z (make-connector)))
    (make-multiplier y z x)
    z))

(define (celsius-fahrenheit-converter x)
  (c+ (c* (c/ (cv 9) (cv 5))
          x)
      (cv 32)))


(define (print-connector c)
  (display (get-value c))(newline))

(define (test-cf-converter)
  (define C (make-connector))
  (define F (celsius-fahrenheit-converter C))
  (make-probe 'F F)
  (set-new-value! C 25 'user)
  (print-connector F)
  )

(test-cf-converter)
