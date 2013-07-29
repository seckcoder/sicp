(import (utils)
        )
(define (monte-carlo trails experiment)
  (define (iter trails-remaining trails-passed)
    (cond ((= 0 trails-remaining)
           (/ trails-passed trails))
          ((experiment)
           (iter (- trails-remaining 1)
                 (+ trails-passed 1)))
          (else
            (iter (- trails-remaining 1)
                  trails-passed))))
  (iter trails 0))


(define (estimate-integral P x1 x2 y1 y2)
  (define (experiment)
    (let ((rand-x (random-in-range x1 x2))
          (rand-y (random-in-range y1 y2)))
      (P rand-x rand-y)))
  (monte-carlo 1000 experiment))

(define (in-unit-square x y)
  (<= (+ (square x) (square y))
      1))


(define (rect-area x1 x2 y1 y2)
  (* (- x2 x1)
     (- y2 y1)))

(define (unit-square-area)
  (let ((l 1.0))
    (* (real->flonum (estimate-integral in-unit-square (- l) l (- l) l))
       (rect-area (- l) l (- l) l))))

(display (unit-square-area))
