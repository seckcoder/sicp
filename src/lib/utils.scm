; basic utilities. This file should not contain dependencies to other files.
(library
  (utils)
  (export square
          set-cadr!
          set-caddr!
          inlist?)
  (import (rnrs)
          (rnrs mutable-pairs))
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
  )
