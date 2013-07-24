(library
  (dict)
  (export make-dict)
  (import (rnrs)
          (rnrs mutable-pairs))
  (define (make-dict)
    (let ((local-table (list '*table*)))
      (define (record-key record) (car record))
      (define (record-value record) (cdr record))
      (define (set-record-value! record value)
        (set-cdr! record value))
      (define (make-record k v) (cons k v))
      (define (lookup k)
        (let ((record (assoc k (cdr local-table))))
          (if record
            (record-value record)
            '())))
      (define (insert! key value)
        (let ((record (assoc key (cdr local-table))))
          (if record
            (set-record-value! record value)
            (set-cdr! local-table (cons (make-record key value)
                                        (cdr local-table))))
          value))
      (define (dispatch key . value)
        (if (null? value)
          (lookup key)
          (insert! key (car value))))
      dispatch))
  )
