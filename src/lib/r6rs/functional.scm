(library
  (functional)
  (export compose
          compose-n
          enumerate-interval
          apply-n)
  (import (rnrs))
  (define (compose f g)
    (lambda (arg0 . rest)
      (f (apply g (cons arg0 rest)))))

  ; compose a function for n times, and return a function that will make
  ; f executed for n times if evaluated.
  (define (compose-n f n)
    (lambda args
      (define (recur k)
        (cond ((> k 0)
               (apply f args)
               (recur (- k 1)))
              (else
                'ok)))
      (recur n)))

  (define (enumerate-interval low high)
    (if (> low high)
      '()
      (cons low (enumerate-interval (+ 1 low) high))))
  ; apply a function for n times with specified arg list
  (define (apply-n f args n)
    (cond ((= n 0) '())
          ((> n 0)
           (cons (apply f args)
                 (apply-n f args (- n 1))))
          (else
            'ignore)))
  )
