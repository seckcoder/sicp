(import (rnrs)
        (stream)
        (utils))

; 3.73 (not really understand it)

(define (rc r c dt)
  (lambda (I v0)
    (stream-add (stream-scale (integral I v0 dt) (/ 1 c))
                (stream-scale I r))
    ))


(define RC1 (rc 5 1 0.5))

(define constant-current (RC1 ones 0))

(stream-display-n constant-current 10)

; 3.74, 3.75, 3.76 unfinished
