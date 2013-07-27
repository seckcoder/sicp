(define (make-monitored proc)
  (define calls 0)
  (lambda args
    (cond ((and (not (null? args))
                (eq? (car args)
                     'how-many-calls?))
           calls)
          (else
            (begin
              (set! calls (+ calls 1))
              (apply proc args))))))

(define s (make-monitored sqrt))
(display (s 100))(newline)
(display (s 'how-many-calls?))(newline)
