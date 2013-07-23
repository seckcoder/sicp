(library
  (base)
  (export operation-table get put
          type-tag contents attach-tag apply-generic
          square
          )
  (import (rnrs)
          (table2d))
  (define operation-table (make-table))
  (define get (operation-table 'lookup-proc))
  (define put (operation-table 'insert-proc!))
  (define coercion-table (make-table))
  (define get-coercion (coercion-table 'lookup-proc))
  (define put-coercion (coercion-table 'insert-proc!))
  (define (attach-tag type-tag contents)
    (cons type-tag contents))
  (define (type-tag datum)
    (if (pair? datum)
      (car datum)
      (error 'type-tag "Bad tagged datum" datum)))
  (define (contents datum)
    (if (pair? datum)
      (cdr datum)
      (error 'contents "Bad tagged datum" datum)))
  (define (square x)
    (* x x))
  (define (apply-generic op . args)
    ; args corresponding to the args of op
    (define (error-msg)
      (error 'apply-generic "No method for these types"
             (list op type-tags)))

    (let ((type-tags (map type-tag args)))
      (let ((proc (get op type-tags)))
        (if proc
          (apply proc (map contents args))
          (if (= (length args) 2)
            (let ((type1 (car type-tags))
                  (type2 (cadr type-tags)))
              (if (eq? type1 type2)
                (error-msg)
                (let ((t1->t2 (get-coercion type1 type2))
                      (t2->t1 (get-coercion type2 type1)))
                  (cond (t1->t2
                          (apply-generic op (t1->t2 a1) a2))
                        (t2->t1
                          (apply-generic op a1 (t2->t1 a2)))
                        (else
                          (error-msg))))))
            (error-msg))))))
  )
