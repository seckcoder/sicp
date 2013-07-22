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
    (let ((type-tags (map type-tag args)))
      (let ((proc (get op type-tags)))
        (if proc
          (apply proc (map contents args))
          (error
            'apply-generic
            "No method for these types"
            (list op type-tags))))))
  )
