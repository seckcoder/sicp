#lang racket

;; continued fraction. Note that for i in ni, i ranges in [1, n]

;; iterative
(define (cont-frac ni di n)
  (define (iter k res)
    (if (= k 0)
      res
      (iter (- k 1)
            (/ (ni k)
               (+ (di k)
                  res)))))
  (iter n 0))


;; recursive
(define (cont-frac-recur ni di n)
  (define (cont-frac-ranged a b)
    (if (> a b)
      0
      (/ (ni a)
         (+ (di a)
            (cont-frac-ranged (+ 1 a)
                              b)))))
  (cont-frac-ranged 1 n))


(provide cont-frac cont-frac-recur)
