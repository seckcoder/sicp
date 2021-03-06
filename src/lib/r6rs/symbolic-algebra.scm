(library
  (symbolic-algebra)
  (export install-polynomial-package
          make-polynomial
          make-term
          variable
          test-symbolic-algebra
          add-terms
          mult-terms
          )
  (import (rnrs)
          (base)
          (functional)
          (generic-arithmetic)
          (utils)
          (poly-terms)
          )

  ; utils
  (define (variable? v) (symbol? v))
  (define (same-variable? v1 v2)
    (and (variable? v1) (variable? v2) (eq? v1 v2)))

  ; polynomials 
  (define (variable p) (car p))
  (define (terms p) (cdr p))

  (define (make-polynomial-inner var terms)
    (cons var terms))
  (define (add-poly p1 p2)
    (if (same-variable? (variable p1)
                        (variable p2))
      (make-polynomial-inner (variable p1)
                             (add-terms (terms p1)
                                        (terms p2)))
      (error 'add-poly "VARIABLES ARE NOT THE SAME")))

  (define (mul-poly p1 p2)
    (if (same-variable? (variable p1)
                        (variable p2))
      (make-polynomial-inner (variable p1)
                             (mult-terms (terms p1)
                                         (terms p2)))
      (error 'mul-poly "VARIABLES ARE NOT THE SAME")))

  (define (negate-poly p)
    (make-polynomial-inner (variable p)
                           (map negate-term
                                (terms p))))



  (define (sub-poly p1 p2)
    (add-poly p1 (negate-poly p2)))

  (define (polynomial-equal-zero? p)
    (let ((p-terms (terms p)))
      (and (not (empty-termlist? p-terms))
           (=zero? (coeff (first-term p-terms)))))
    )
  (define (beautiful-display-polynomial p)
    (if (not (empty-termlist? (terms p)))
      (begin
        (display "(")
        (beautiful-display-terms (variable p) (terms p))
        (display ")"))))

  (define (display-terms termlst)
    (if (not (empty-termlist? termlst))
      (begin
        (display (first-term termlst))
        (display ":")
        (display (order (first-term termlst)))
        (newline)
        )
      (begin
        (display termlst)(newline))))

  ; terms are ordered by order desc.
  (define (add-terms L1 L2)
    (cond ((empty-termlist? L1) L2)
          ((empty-termlist? L2) L1)
          ((> (order (first-term L1))
              (order (first-term L2)))
           (begin 
             (adjoin-term (first-term L1)
                          (add-terms (rest-terms L1)
                                     L2)))
           )
          ((< (order (first-term L1))
              (order (first-term L2)))
           (adjoin-term (first-term L2)
                        (add-terms L1
                                   (rest-terms L2))))
          (else (begin 
                  (adjoin-term (add-term (first-term L1)
                                         (first-term L2))
                               (add-terms (rest-terms L1)
                                          (rest-terms L2))))))
    )

  ; Not ues fold-right here since fold-right have an assumption 
  ; on the representation of terms(first-term = car)
  (define (mult-terms L1 L2)
    (if (empty-termlist? L1)
      L1
      (begin
        (add-terms (mult-term-terms (first-term L1) L2)
                   (mult-terms (rest-terms L1) L2)))))

  (define (mult-term-terms t termlst)
    (if (empty-termlist? termlst)
      termlst
      (adjoin-term (mult-term t (first-term termlst))
                   (mult-term-terms t (rest-terms termlst)))))

  (define (negate-term term)
    (make-term (order term)
               (negate (coeff term))))

  ; add term that has the same order
  (define (add-term t1 t2)
    (if (= (order t1)
           (order t2))
      (make-term (order t1)
                 (add (coeff t1)
                      (coeff t2)))
      (error 'add-term "ORDER IS NOT EQUAL")))
  ; mult two terms
  (define (mult-term t1 t2)
    (make-term (+ (order t1)
                  (order t2))
               (mul (coeff t1)
                    (coeff t2))))

  (define (install-polynomial-package)
    (define (tag x) (attach-tag 'polynomial x))
    (put 'add '(polynomial polynomial) (compose tag add-poly))
    (put 'mul '(polynomial polynomial) (compose tag mul-poly))
    (put 'sub '(polynomial polynomial) (compose tag sub-poly))
    (put 'negate 'polynomial (compose tag negate-poly))
    (put 'make 'polynomial (compose tag make-polynomial-inner))
    (put '=zero? 'polynomial polynomial-equal-zero?)
    (put 'display 'polynomial beautiful-display-polynomial)
    (declare-type 'polynomial '())
    )

  (define (make-polynomial var terms)
    ((get-or-fail 'make 'polynomial) var terms))

  (define (test-symbolic-algebra)
    (define poly13 
      (make-polynomial 'x (make-integer-numbers '(1 2 0 3 -2 -5))))
    (define poly1
      (make-polynomial 'x (make-integer-numbers '(1 2))))
    (beautiful-display (mul poly13 poly1))
    )
  )
