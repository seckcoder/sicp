(library
  (symbolic-algebra)
  (export install-polynomial-package
          make-polynomial
          make-term)
  (import (rnrs)
          (base)
          (functional)
          )

  (define (make-term order coeff) (cons coeff order))
  (define (variable? v) (symbol? v))
  (define (same-variable? v1 v2)
    (and (variable? v1) (variable? v2) (eq? v1 v2)))

  (define (install-polynomial-package)
    (define (make-polynomial var terms) (cons var terms))
    (define (add-poly p1 p2)
      (if (same-variable? (variable p1)
                          (variable p2))
        (make-polynomial (variable p1)
                         (add-terms (terms p1)
                                    (terms p2)))
        (error 'add-poly "VARIABLES ARE NOT THE SAME")))

    (define (mul-poly p1 p2)
      (if (same-variable? (variable p1)
                          (variable p2))
        (make-polynomial (variable p1)
                         (mult-terms (terms p1)
                                     (terms p2)))
        (error 'mul-poly "VARIABLES ARE NOT THE SAME")))
    (define (variable p) (car p))
    (define (terms p) (cdr p))

    ; terms are ordered by order desc.
    (define (add-terms L1 L2)
      (cond ((empty-termlist? L1) L2)
            ((empty-termlist? L2) L1)
            ((> (order (first-term L1))
                (order (first-term L2)))
             (adjoin-term (first-term L1)
                          (add-terms (rest-terms L1)
                                     L2)))
            ((< (order (first-term L1))
                (order (first-term L2)))
             (adjoin-term (first-term L2)
                          (add-terms L1
                                     (rest-terms L2))))
            (else (adjoin-term (add-term (first-term L1)
                                         (first-term L2))
                               (add-terms (rest-terms L1)
                                          (rest-terms L2))))))

    (define (mult-terms L1 L2)
      (fold-right add-terms
                  (the-empty-termlist)
                  (map (lambda (t1)
                         (mult-term-terms t1 L2)) L1)))

    ; multiply a term with a list of terms
    (define (mult-term-terms t termlst)
      (map (lambda (t1)
             (mult-term t t1)) termlst))

    (define (the-empty-termlist) '());
    (define (empty-termlist? L) (null? L));
    (define (adjoin-term t termlst)
      (if (=zero? (coeff t))
        termlst
        (cons t termlst)))
    (define (first-term termlst) (car termlst))
    (define (rest-terms termlst) (cdr termlst))
    (define (order term) (cdr term))
    (define (coeff term) (car term))
    ; add term that has the same order
    (define (add-term t1 t2)
      (make-term (order t1)
                 (add (coeff t1)
                      (coeff t2))))
    ; mult two terms
    (define (mult-term t1 t2)
      (make-term (+ (order t1)
                    (order t2))
                 (mul (coeff t1)
                      (coeff t2))))
    (define (tag x) (attach-tag 'polynomial x))
    (put 'add '(polynomial polynomial) (compose tag add-poly))
    (put 'mul '(polynomial polynomial) (compose tag mul-poly))
    (put 'make 'polynomial (compose tag make-polynomial))
    )

  (define (make-polynomial var terms)
    ((get 'make 'polynomial) var terms))
  )
