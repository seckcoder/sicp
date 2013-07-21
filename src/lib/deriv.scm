(library
  (deriv)
  (export install-deriv-package deriv)
  (import (rnrs)
          (base))
  (define (operator exp)
    (car exp))

  (define (operands exp)
    (cdr exp))

  (define (variable? x) (symbol? x))

  (define (same-variable? v1 v2)
    (and (variable? v1) (variable? v2) (eq? v1 v2)))

  (define (sum? x)
    (and (pair? x) (eq? (car x) '+)))

  (define (addend s) (cadr s))

  (define (augend s)
    (cond ((null? (cdddr s)) (caddr s))
          (else (cons '+ (cddr s)))))

  (define (product? x)
    (and (pair? x) (eq? (car x) '*)))

  (define (multiplier p) (cadr p))

  (define (multiplicand p)
    (cond ((null? (cdddr p)) (caddr p))
          (else (cons '* (cddr p)))))

  (define (exponentiation? s)
    (and (pair? s) (eq? (car s) '**)))

  (define (base s) (cadr s))

  (define (exponent s) (caddr s))

  (define (=number? exp num)
    (and (number? exp) (= exp num)))

  (define (make-sum a1 . a2)
    (let ((lst-of-args (cons a1 a2)))
      (let ((constant (apply + (filter number? lst-of-args)))
            (rest (filter (lambda (s) (not (number? s))) lst-of-args)))
        (cond ((null? rest) constant)
              ((and (= constant 0) (null? (cdr rest))) (car rest))
              ((= constant 0) (cons '+ rest))
              (else (append (list '+ constant) rest))))))

  (define (make-product m1 . m2)
    (let ((lst-of-args (cons m1 m2)))
      (let ((constant (apply * (filter number? lst-of-args)))
            (rest (filter (lambda (s) (not (number? s))) lst-of-args)))
        (cond ((= constant 0) 0)
              ((null? rest) constant)
              ((and (= constant 1) (null? (cdr rest))) (car rest))
              ((= constant 1) (cons '* rest))
              (else (append (list '* constant) rest))))))

  (define (make-exponentiation base exponent)
    (cond ((=number? exponent 0) 1)
          ((=number? exponent 1) base)
          (else (list '** base exponent))))

  (define (deriv exp var)
    (cond ((number? exp) 0)
          ((variable? exp)
           (if (same-variable? exp var) 1 0))
          (else ((get 'deriv (operator exp)) exp
                                             var))))

  (define (install-deriv-package)

    (put 'deriv '+ (lambda (exp var)
                     (make-sum (deriv (addend exp) var)
                               (deriv (augend exp) var))))

    (put 'deriv '* (lambda (exp var)
                     (make-sum
                       (make-product (multiplier exp)
                                     (deriv (multiplicand exp) var))
                       (make-product (deriv (multiplier exp) var)
                                     (multiplicand exp)))))

    (put 'deriv '** (lambda (exp var)
                      (make-product (exponent exp)
                                    (make-exponentiation (base exp)
                                                         (make-sum (exponent exp)
                                                                   -1))
                                    (deriv (base exp) var))))

    )

  )
