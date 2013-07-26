(library
  (complex-rectangular)
  (export install-rectangular-package)
  (import (rnrs)
          (base)
          (functional))

  (define (install-rectangular-package)
    (define (myreal-part z) (car z))
    (define (myimag-part z) (cdr z))
    (define (make-from-real-imag x y) (cons x y))
    (define (mymagnitude z)
      (sqrt (+ (square (myreal-part z))
               (square (myimag-part z)))))
    (define (myangle z)
      (atan (myimag-part z) (myreal-part z)))
    (define (make-from-mag-ang r a)
      (cons (* r (cos a)) (* r (sin a))))
    (define (tag x) (attach-tag 'rectangular x))
    (put 'real-part '(rectangular) myreal-part)
    (put 'imag-part '(rectangular) myimag-part)
    (put 'magnitude '(rectangular) mymagnitude)
    (put 'angle '(rectangular) myangle)
    (put 'make-from-real-imag 'rectangular
         (compose tag make-from-real-imag))
    (put 'make-from-mag-ang 'rectangular
         (compose tag make-from-mag-ang))
    'ok)
  )