#lang racket


(require "deriv.rkt")

(deriv '(+ x 3) 'x)

(deriv '(* x y) 'x)

(deriv '(* (* x y) (+ x 3)) 'x)

(deriv '(** x 1) 'x)
(deriv '(** x 1) 'v)
(deriv '(** x 3) 'x)
(deriv '(** x n) 'x)
