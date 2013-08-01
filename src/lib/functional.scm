(library
  (functional)
  (export compose
          compose-n)
  (import (rnrs))
  (define (compose f g)
    (lambda (arg0 . rest)
      (f (apply g (cons arg0 rest)))))
  (define (compose-n f n)
    (lambda args
      (define (recur k)
        (cond ((> k 0)
               (apply f args)
               (recur (- k 1)))
              (else
                'ok)))
      (recur n)))
  )
