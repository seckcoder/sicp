; basic utilities. This file should not contain dependencies to other files.
(library
  (utils)
  (export square
          cube
          set-cadr!
          set-caddr!
          inlist?
          range
          divides?
          divisible? 
          println
          average
          pythagorean-triple?)

  (import (rnrs)
          (rnrs r5rs)
          (rnrs mutable-pairs))
  (define (set-cadr! lst v)
    (set-car! (cdr lst) v))

  (define (set-caddr! lst v)
    (set-car! (cdr (cdr lst)) v))

  (define (square x)
    (* x x))

  (define (cube x)
    (* x x x))

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

  (define (divides? a b)
    (= (remainder b a) 0))

  (define (divisible? x y)
    (= (remainder x y) 0))

  (define (println v)
    (display v)(newline)
    v)

  (define (average a b)
    (/ (+ a b) 2.0))

  (define (pythagorean-triple? i j k)
    (= (square k)
       (+ (square i)
          (square j))))
  )
