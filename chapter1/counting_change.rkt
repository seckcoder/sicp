#lang racket

(define (count-change sum num-changes first-denomination)
  (cond ((= sum 0) 1)  ;; we have got a method to make change
        ((or (= num-changes 0) (< sum 0)) 0)
        (else (+ (count-change sum
                               (- num-changes 1)
                               first-denomination)
                 (count-change (- sum (first-denomination num-changes))
                               num-changes
                               first-denomination)))))

(define (simple-cc sum)
  (count-change sum 5 denomination-example))

(define (denomination-example num-changes)
  (cond ((= num-changes 1) 1)
        ((= num-changes 2) 5)
        ((= num-changes 3) 10)
        ((= num-changes 4) 25)
        ((= num-changes 5) 50)))


(provide count-change simple-cc)
