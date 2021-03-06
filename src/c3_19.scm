(define (cycle? lst)
  (define (catch-up fast slow)
    (cond ((or (null? fast)
              (null? slow)) #f)
          ((eq? fast slow) #t)
          (else (catch-up (list-walk 2 fast)
                          (list-walk 1 slow)))))
  (catch-up (list-walk 2 lst)
            (list-walk 1 lst)))

; let the list walk for `step` num steps
(define (list-walk step lst)
  (cond ((null? lst) '())
        ((= step 0) lst)
        (else (list-walk (sub1 step)
                         (cdr lst)))))

(define cycle '(1 2 3))
(set-cdr! (last-pair cycle) cycle)
(display (cycle? cycle))
(display (cycle? '(1 2 3)))
