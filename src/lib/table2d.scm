(library
  (table2d)
  (export make-table)
  (import (rnrs)
          (rnrs mutable-pairs))
  (define (make-table)
    (let ((local-table (list '*table*)))
      (define (lookup key-1 key-2)
        (let ((subtable (assoc key-1 (cdr local-table))))
          (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                (cdr record)
                #f))
            #f)))
      (define (insert! key-1 key-2 value)
        (let ((subtable (assoc key-1 (cdr local-table))))
          (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                (set-cdr! record value)
                (set-cdr! subtable
                          (cons (cons key-2 value)
                                (cdr subtable)))))
            (set-cdr! local-table
                      (cons (list key-1
                                  (cons key-2 value))
                            (cdr local-table))))
          'ok))
      (define (dispatch m)
        (cond ((eq? m 'lookup-proc) lookup)
              ((eq? m 'insert-proc!) insert!)
              (else (error 'table-dispatch "Unknown operation" m))))
      dispatch)))
