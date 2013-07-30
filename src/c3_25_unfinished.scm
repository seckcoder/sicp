(define (make-table)
  (let ((local-table (list '*table*)))
    ; table is record, record is table
    (define (record-key record) (car record))
    (define (record-value record) (cdr record))
    (define (set-record-value! record value) (set-cdr! record value))
    (define (make-record k v) (cons k v))

    ; add a record at the head of the table's list of records
    (define (add-record! table record)
      (set-record-value! table
                         (cons record
                               (record-value table)))
      table)

    (define (lookup . keys)
      (define (lookup-recur keys table)
        (cond ((not (pair? table))
               ; too many keys
               #f)
              ((null? keys)
               ; key matched (maybe partial match)
               (record-value table))
              (else
                (let ((subtable (assoc (car keys) (record-value table))))
                  (if subtable
                    (lookup-recur (cdr keys) subtable)
                    #f)))))
      (lookup-recur keys local-table))

    (define (build-table-keys-value keys value)
      (cond ((null? keys) value)
            (else
              (cons (car keys)
                    (build-table-keys-value (cdr keys)
                                            value)))))

    (define (insert! value . keys)
      (define (insert-table! keys value table)
        (cond ((null? keys)
               (set-cdr! table value))
              (else
                (let ((subtable (assoc (car keys) (cdr table))))
                  (if subtable
                    (insert-table! (cdr keys) value subtable)
                    (add-record! table
                                 (build-table-keys-value keys value)))))))
      (insert-table! keys value local-table))

    (define (record? t)
      (if (not (pair? t))
        (error 'record? "WTF")
        (atom? (cdr t))))

    (define (print-table)
      (define (recur table)
        (cond ((not (pair? table))
               (error 'print-table "WTF"))
              ((record? table)
               (display (cdr table)))
              (else
                (map (lambda (sub-table)
                       (recur sub-table))
                     (cdr tabe)))))
      (recur local-table))

    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error 'table-dispatch "Unknown operation" m))))
    (insert! 3 1 2)
    (print-table)
    dispatch)
  )
(make-table)
