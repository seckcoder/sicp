(define (make-accumulator sum)
  (lambda (augend)
    (set! sum (+ sum augend))
    sum))

(define A (make-accumulator 5))
(display (A 10))(newline)
(display (A 10))(newline)
(display (A 20))(newline)
