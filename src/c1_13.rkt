#lang racket

;; First prove that:
;; suppose a = (1 + √5) / 2, b = (1 - √5) / 2
;; then it's trivial to prove that
;; a^n - b^n + a^(n+1) - b^(n+1) = a^(n+2) - b^(n+2)
;; then use mathematical induction prove Fib(n) = (a^n - b^n)/√5
