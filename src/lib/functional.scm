(library
  (functional)
  (export compose)
  (import (rnrs))
  (define (compose f g)
    (lambda (arg0 . rest)
      (f (apply g (cons arg0 rest)))))
  )
