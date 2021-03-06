(define (count-pairs x)
  (define (recur x counted-pairs)
    (if (or (not (pair? x))
            (set-find counted-pairs x))
      (list 0 counted-pairs)
      (let ((car-result (recur (car x) (cons x counted-pairs))))
        (let ((car-count (car car-result))
              (car-counted-pairs (cadr car-result)))
          (let ((cdr-result (recur (cdr x) car-counted-pairs)))
            (let ((cdr-count (car cdr-result))
                  (cdr-counted-pairs (cadr cdr-result)))
              (list (+ car-count
                       cdr-count
                       1)
                    cdr-counted-pairs)))))))
  (car (recur x '())))

(define (set-find s x)
  (cond ((null? s) #f)
        ((eq? (car s) x) #t)
        (else (set-find (cdr s) x))))

(define cycle '(1 2 3))
(set-cdr! (last-pair cycle) cycle)
(display (count-pairs cycle))
