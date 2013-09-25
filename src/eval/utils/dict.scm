(define (make-dict)
  (let ((local-table (list '*table*)))
    (define (record-key record) (car record))
    (define (record-value record) (cdr record))
    (define (set-record-value! record value)
      (set-cdr! record value))
    (define (make-record k v) (cons k v))
    (define (empty? table)
      (null? (cdr table)))
    (define (lookup-record k)
      (define (iter table-records)
        (cond ((null? table-records)
               ; not found
               (list #f '()))
              ((eq? k (record-key (car table-records)))
               (list #t (car table-records)))
              (else
                (iter (cdr table-records)))))
      (iter (cdr local-table)))

    ; return (list found record)
    (define (lookup k)
      (let* ((ret (lookup-record k))
             (found (car ret))
             (record (cadr ret)))
        (if found
          (list #t (record-value record))
          (list #f '()))
        ))

    ; return true when insert a new record,
    ; return false when a record is modified
    (define (insert! key value)
      (let* ((ret (lookup-record key))
             (found (car ret))
             (record (cadr ret)))
        (if found
          (set-record-value! record value)
          (set-cdr! local-table (cons (make-record key value)
                                      (cdr local-table))))
        (not found)))

    ; return true when the record is found and set
    ; return false when the record is not found.
    ; difference between set! and insert! is that insert! will
    ; insert a new record when the record doesn't exist.
    (define (set! key value)
      (let* ((ret (lookup-record key))
             (found (car ret))
             (record (cadr ret)))
        (if found
          (set-record-value! record value)
          #f)))

    (define (keys)
      (map car (cdr local-table)))

    ; randomly return a key
    (define (akey)
      (record-key (car (cdr local-table))))

    (define (dispatch action)
      (cond ((eq? action 'lookup) lookup)
            ((eq? action 'insert) insert!)
            ((eq? action 'set) set!)
            ((eq? action 'keys) keys)
            ((eq? action 'akey) akey)
            (else (error 'dict-dispatch "UNKNOWN ACTION" action))))
    dispatch))

(define (dict-lookup d key)
  ((d 'lookup) key))

(define (dict-insert! d key value)
  ((d 'insert) key value))

(define (dict-set! d key value)
  ((d 'set) key value))
