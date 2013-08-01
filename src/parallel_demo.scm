(import (functional)
        (concurrency)
        (utils))

(define x 1)
(define (add1-x)
  (display "add1-x ")(newline)
  (set! x (+ x 1)))
(define (mul2)
  (display "mul2 ")(newline)
  (set! x (* 2 x)))

; compose the fuction for n times

((compose-n add1-x 10))
((compose-n mul2 10))
(display x)(newline)

(define (parallel-test)
  (set! x 1)
  (parallel-execute (compose-n add1-x 10)
                    (compose-n mul2 10))
  (sleepfor 1)
  (display x))

(parallel-test)
