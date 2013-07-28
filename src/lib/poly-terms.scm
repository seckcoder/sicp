(library
  (poly-terms)
  (export install-sparse-poly-terms-package
          install-dense-poly-terms-spackage
          first-term
          rest-terms
          empty-termlist?
          make-termlist
          adjoin-term
          make-term
          order
          coeff
          beautiful-display-terms)
  (import (rnrs)
          (base)
          (utils)
          (functional)
          (generic-arithmetic))

  (define (order term) (cdr term))
  (define (coeff term) (car term))
  (define (make-term order coeff) (cons coeff order))
  ; terms
  (define (beautiful-display-term variable term)
    ; beautiful-display-term-coeff
    (let ((term-coeff (coeff term))
          (term-order (order term)))
      (cond ((=zero? term-coeff)
             (display 0))
            ((zero? term-order)
             (beautiful-display term-coeff))
            (else (begin
                    (if (not (equal-int1? term-coeff))
                      (beautiful-display term-coeff))
                    (display variable)
                    (if (> term-order 1)
                      (begin
                        (display "^")
                        (display term-order))))))))
  (define (beautiful-display-terms variable termlst)
    (if (not (empty-termlist? termlst))
      (begin
        (let ((p-first-term (first-term termlst))
              (p-rest-terms (rest-terms termlst)))
          (beautiful-display-term variable p-first-term)
          (if (not (empty-termlist? p-rest-terms))
            (begin
              (display "+")
              (beautiful-display-terms variable p-rest-terms))))
        )))

  (define (install-sparse-poly-terms-package)
    (define (first-term termlst) (car termlst))
    (define (rest-terms termlst) (cdr termlst))
    (define (adjoin-term t termlst)
      (if (=zero? (coeff t))
        termlst
        (cons t termlst)))
    (define (the-empty-termlist) '())
    (define (make-sparse-terms list-of-terms)
      (fold-right (lambda (term termlst)
                    (adjoin-term term termlst))
                  (the-empty-termlist)
                  list-of-terms))

    (define (tag termlst) (attach-tag 'sparse termlst))
    (define (empty-termlist? L) (null? L))
    (put 'first-term '(sparse) first-term)
    (put 'rest-terms '(sparse) (compose tag rest-terms))
    (put 'adjoin-term 'sparse (compose tag adjoin-term))
    (put 'the-empty-termlist 'sparse (compose tag the-empty-termlist))
    (put 'empty-termlist? 'sparse empty-termlist?)
    (put 'make 'sparse (compose tag make-sparse-terms))
    )

  (define (install-dense-poly-terms-spackage)
    (define (first-term termlst) (make-term (- (length termlst) 1)
                                            (car termlst)))
    (define (rest-terms termlst) (cdr termlst))
    (define (adjoin-term t termlst)
      (define (cons-term)
        (cons (coeff t) (adjoin-term (make-term (- (order t) 1)
                                                (make-integer-number 0))
                                     termlst)))
      (cond ((and (empty-termlist? termlst)
                  (zero? (order t)))
             (cons (coeff t) termlst))
            ((empty-termlist? termlst)
             (cons-term))
            (else
              (let ((termlst-ft (first-term termlst)))
                (cond ((= (- (order t)
                             (order termlst-ft))
                          1)
                       (cons-term))
                      ((> (- (order t)
                             (order termlst-ft))
                          1)
                       (cons-term))
                      (else termlst))))))
    (define (the-empty-termlist) '())
    (define (empty-termlist? L) (null? L))
    (define (make-dense-terms list-of-terms)
      (fold-right (lambda (term termlst)
                    (adjoin-term term termlst))
                  (the-empty-termlist)
                  list-of-terms))

    (define (tag termlst) (attach-tag 'dense termlst))
    (put 'first-term '(dense) first-term)
    (put 'rest-terms '(dense) rest-terms)
    (put 'adjoin-term 'dense adjoin-term)
    (put 'the-empty-termlist 'dense (compose tag the-empty-termlist))
    (put 'empty-termlist? 'dense empty-termlist?)
    (put 'make 'dense (compose tag make-dense-terms)))

  (define (first-term termlst)
    (apply-generic 'first-term termlst))
  (define (rest-terms termlst)
    (apply-generic 'rest-terms termlst))
  (define (adjoin-term t termlst)
    ((get 'adjoin-term (type-tag termlst)) t (contents termlst)))
  (define (empty-termlist? termlst)
    ((get 'empty-termlist? (type-tag termlst)) (contents termlst)))
  (define (make-termlist type . args)
    (apply (get 'make type) args))
  )
