(library
  (complex)
  (export myreal-part myimag-part mymagnitude myangle
          make-from-real-imag make-from-mag-ang)
  (import (rnrs)
          (base))
  (define (myreal-part z) (apply-generic 'real-part z))
  (define (myimag-part z) (apply-generic 'imag-part z))
  (define (mymagnitude z) (apply-generic 'magnitude z))
  (define (myangle z) (apply-generic 'angle z))
  (define (make-from-real-imag x y)
    (let ((proc (get 'make-from-real-imag 'rectangular)))
      (if proc
        (proc x y)
        (error 'make-from-real-imag "cannot find proc"))))
  (define (make-from-mag-ang r a)
    (let ((proc (get 'make-from-mag-ang 'polar)))
      (if proc
        (proc r a)
        (error 'make-from-mag-ang "cannot find proc"))))
  )
