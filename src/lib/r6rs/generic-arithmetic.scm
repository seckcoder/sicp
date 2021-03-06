(library
  (generic-arithmetic)
  (export install-integer-package
          install-rational-package
          install-real-package
          install-complex-package
          make-integer-number
          make-integer-numbers
          make-real-number
          make-rational-number
          make-complex-from-real-imag
          make-complex-from-mag-ang
          )

  (import (rnrs)
          (rnrs mutable-pairs)
          (base)
          (complex)
          (functional)
          )

  (define (make-integer-number n)
    ((get 'make 'integer) n))

  (define (make-integer-numbers list-of-n)
    (map make-integer-number list-of-n))

  (define (make-real-number n)
    ((get 'make 'real) n))

  (define (install-number-type-package number-type)
    (define (tag x)
      (attach-tag number-type x))
    (put 'add (list number-type number-type)
         (compose tag +))
    (put 'sub (list number-type number-type)
         (compose tag -))
    (put 'mul (list number-type number-type)
         (compose tag *))
    (put 'div (list number-type number-type)
         (compose tag /))
    (put 'negate number-type
         (compose tag -))
    (put 'equ? (list number-type number-type) =)
    (put 'display number-type display)
    (put 'make number-type tag)
    )

  (define (install-integer-package)
    (define (integer->rational intv)
      (make-rational-number intv 1))
    (install-number-type-package 'integer)
    (put 'raise 'integer integer->rational)
    (put 'zero 'integer (make-integer-number 0))
    (declare-type 'integer 'rational)
    )

  (define (install-real-package)
    (define (real->complex realv)
      (make-complex-from-real-imag realv 0))
    (define (real->rational x)
      (define (recur flonum-numer int-denom)
        ; we expect that 15.0 = 15
        (if (= (flonum->fixnum flonum-numer)
               flonum-numer)
          (make-rational-number (flonum->fixnum flonum-numer)
                                int-denom)
          (recur (* 10 flonum-numer)
                 (* 10 int-denom))))
      (recur x 1))
    (install-number-type-package 'real)
    (put 'raise 'real real->complex)
    (put 'zero 'real (make-real-number 0.0))
    (put 'project 'real real->rational)
    (declare-type 'real 'complex)
    )

  (define (install-rational-package)
    (define (numer x) (car x))
    (define (denom x) (cdr x))
    (define (make-rat n d)
      (let ((g (gcd n d)))
        (cons (/ n g) (/ d g))))
    (define (add-rat x y)
      (make-rat (+ (* (numer x) (denom y))
                   (* (numer y) (denom x)))
                (* (denom x) (denom y))))
    (define (sub-rat x y)
      (make-rat (- (* (numer x) (denom y))
                   (* (numer y) (denom x)))
                (* (denom x) (denom y))))
    (define (mul-rat x y)
      (make-rat (* (numer x) (numer y))
                (* (denom x) (denom y))))
    (define (div-rat x y)
      (make-rat (* (numer x) (denom y))
                (* (denom x) (numer y))))

    (define (negate-rat x)
      (make-rat (- (numer x))
                (denom x)))

    (define (eq-rat x y)
      (and (= (numer x) (numer y))
           (= (denom x) (denom y))))

    (define (rational->real rationalv)
      (make-real-number (real->flonum
                          (/ (numer rationalv)
                             (denom rationalv)))))

    (define (rational->integer x)
      (make-integer-number (flonum->fixnum (real->flonum
                                             (/ (numer x)
                                                (denom x))))))

    ;; interface to rest of the system
    (define (tag x) (attach-tag 'rational x))
    (put 'add '(rational rational) (compose tag add-rat))
    (put 'sub '(rational rational) (compose tag sub-rat))
    (put 'mul '(rational rational) (compose tag mul-rat))
    (put 'div '(rational rational) (compose tag div-rat))
    (put 'negate 'rational (compose tag negate-rat))
    (put 'equ? '(rational rational) eq-rat)
    (put 'make 'rational (compose tag make-rat))
    (put 'zero 'rational (make-rational-number 0 1))
    (put 'raise 'rational rational->real)
    (put 'project 'rational rational->integer)
    (put 'display 'rational (lambda (x)
                              (display "(")
                              (display (numer x))
                              (display "/")
                              (display (denom x))
                              (display ")")))
    (declare-type 'rational 'real)
    )

  (define (make-rational-number n d)
    ((get 'make 'rational) n d))

  (define (install-complex-package)
    (define (make-from-real-imag x y)
      ((get 'make-from-real-imag 'rectangular) x y))
    (define (make-from-mag-ang r a)
      ((get 'make-from-mag-ang 'polar) r a))
    ;; internal procedures
    (define (add-complex z1 z2)
      (make-from-real-imag (+ (myreal-part z1) (myreal-part z2))
                           (+ (myimag-part z1) (myimag-part z2))))
    (define (sub-complex z1 z2)
      (make-from-real-imag (- (myreal-part z1) (myreal-part z2))
                           (- (myimag-part z1) (myimag-part z2))))
    (define (mul-complex z1 z2)
      (make-from-mag-ang (* (mymagnitude z1) (mymagnitude z2))
                         (+ (myangle z1) (myangle z2))))
    (define (div-complex z1 z2)
      (make-from-mag-ang (/ (mymagnitude z1) (mymagnitude z2))
                         (- (myangle z1) (myangle z2))))
    (define (eq-complex z1 z2)
      (and (= (myreal-part z1) (myreal-part z2))
           (= (myimag-part z1) (myimag-part z2))))
    (define (negate-complex z)
      (make-from-real-imag (- (myreal-part z))
                           (- (myimag-part z))))

    (define (complex->real z)
      (make-real-number (myreal-part z)))

    ;; imported procedures from rectangular and polar packages
    ;; interface to rest of the system
    (define (tag z) (attach-tag 'complex z))
    (put 'add '(complex complex) (compose tag add-complex))
    (put 'sub '(complex complex) (compose tag sub-complex))
    (put 'mul '(complex complex) (compose tag mul-complex))
    (put 'div '(complex complex) (compose tag div-complex))
    (put 'equ? '(complex complex) eq-complex)
    (put 'make-from-real-imag 'complex
         (compose tag make-from-real-imag))
    (put 'make-from-mag-ang 'complex
         (compose tag make-from-mag-ang))
    (put 'real-part '(complex) myreal-part)
    (put 'imag-part '(complex) myimag-part)
    (put 'magnitude '(complex) mymagnitude)
    (put 'angle '(complex) myangle)
    (put 'zero 'complex (make-complex-from-real-imag 0 0))
    (put 'project 'complex complex->real)
    (put 'display 'complex (lambda (x)
                             (display "(")
                             (display (myreal-part x))
                             (display "+")
                             (display (myimag-part x))
                             (display "i")
                             (display ")")))
    )

  (define (make-complex-from-real-imag x y)
    ((get 'make-from-real-imag 'complex) x y))

  (define (make-complex-from-mag-ang r a)
    ((get 'make-from-mag-ang 'complex) r a))
  )
