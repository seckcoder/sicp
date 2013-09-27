(define (assert exp)
  (if (not exp)
    (error 'assert "expression is not true")
    exp)
  )

(define (print . args)
  (define (iter args)
    (if (not (null? args))
      (begin
        (display (car args))(display " ")
        (iter (cdr args))
        )))
  (iter args))

(define (println . args)
  (apply print args)(newline)
  )
