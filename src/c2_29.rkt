#lang racket

(require "base.rkt")
(require "mobile.rkt")

(define (branch-weight branch)
  (total-weight (branch-structure branch)))

;; (a)
(define (total-weight mobile)
  (if (atom? mobile)
    mobile
    (+ (branch-weight (left-branch mobile))
       (branch-weight (right-branch mobile)))))

(define (branch-torque branch)
  (* (branch-length branch)
     (branch-weight branch)))

;; (b)
(define (balanced? mobile)
  (if (atom? mobile)
    true
    (and (balanced? (branch-structure (left-branch mobile)))
         (balanced? (branch-structure (right-branch mobile)))
         (= (branch-torque (left-branch mobile))
            (branch-torque (right-branch mobile))))))

;; (c)
; replace mobile1.rkt with mobile.rkt
; and we have no need to change code in c2_29.rkt.
; we just need to modify the left-branch/right-branch/branch-length..
; in mobile.rkt

(define (demo-mobile mobile1)
  (display mobile1)
  (newline)
  (display (total-weight mobile1))
  (newline)
  (display (balanced? mobile1))
  (newline))

(define (demo)
  (define br1 (make-branch 1 3))
  (define br2 (make-branch 2 4))
  (define br3 (make-branch 3
                           (make-mobile br1
                                        br2)))
  (define br4 (make-branch 4 5))
  (define mobile1 (make-mobile br4 br3))
  (define mobile2 (let* ((br1 (make-branch 4 5))
                         (br2 (make-branch 1 8))
                         (br3 (make-branch 4 2))
                         (br4 (make-branch 2
                                           (make-mobile br2
                                                        br3))))
                    (make-mobile br4
                                 br1)))

  (demo-mobile mobile1)
  (demo-mobile mobile2))
(demo)
