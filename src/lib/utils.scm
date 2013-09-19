; basic utilities. This file should not contain dependencies to other files.
(library
  (utils)
  (export square
          set-cadr!
          set-caddr!
          inlist?
          range
          random-in-range
          sleepfor
          divides?)
  (import (chezscheme))
  (define (set-cadr! lst v)
    (set-car! (cdr lst) v))

  (define (set-caddr! lst v)
    (set-car! (cdr (cdr lst)) v))

  (define (square x)
    (* x x))

  (define (inlist? x lst eqfn)
    (if (filter (lambda (y)
                  (eqfn x y))
                lst)
      #t
      #f))

  ; [a b)
  (define (range a b)
    (cond ((= a b) '())
          ((> a b) (cons a (range (- a 1) b)))
          ((< a b) (cons a (range a (- b 1))))))

  ; [low high)
  (define (random-in-range low high)
    (let ((range (- high low)))
      (+ low (random range))))

  ; sleep for s seconds
  (define (sleepfor s)
    (sleep (make-time 'time-duration 0 s)))
  (define (divides? a b)
    (= (remainder b a) 0))

  )
