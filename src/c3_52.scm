(import (rnrs)
        (stream)
        (utils))

(let ((sum 0))
  (define (accum x)
    (set! sum (+ x sum))
    sum)
  ; ok, assignment in lazy evaluation is really confusing
  (let ((seq (stream-map accum (stream-enumerate-interval 1 20))))
    (define y (stream-filter even? seq))
    (define z (stream-filter (lambda (x) (= (remainder x 5) 0))
                             seq))
    (println (stream-ref y 7))
    (newline)
    (stream-display z)))

;; the results is different. when memoize enabled, accum will be
;; called for less times, and resulting in smaller values printed.
