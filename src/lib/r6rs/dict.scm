(library
  (dict)
  (export make-dict
          dict-lookup
          dict-insert!)
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
      (define (keys)
        (map car (cdr local-table)))

      ; randomly return a key
      (define (akey)
        (record-key (car (cdr local-table))))
               
      (define (dispatch action)
        (cond ((eq? action 'lookup) lookup)
              ((eq? action 'insert) insert!)
              ((eq? action 'keys) keys)
              ((eq? action 'akey) akey)
              (else (error 'dict-dispatch "UNKNOWN ACTION" action))))
      dispatch))

  (define (dict-lookup d key)
    ((d 'lookup) key))

  (define (dict-insert! d key value)
    ((d 'insert) key value))
  )
