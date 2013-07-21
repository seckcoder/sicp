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
      (error "Bad tagged datum -- TYPE-TAG" datum)))
  (define (contents datum)
    (if (pair? datum)
      (cdr datum)
      (error "Bad tagged datum -- CONTENTS" datum)))
  (define (square x)
    (* x x))
  (define (apply-generic op . args)
    ; args corresponding to the args of op
    (let ((type-tags (map type-tag args)))
      (let ((proc (get op type-tags)))
        (if proc
          (apply proc (map contents args))
          (error
            "No method for these types -- APPLY-GENERIC"
            (list op type-tags))))))
  )
