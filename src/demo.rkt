#lang racket


(require "deriv.rkt")

(deriv '(+ x 3) 'x)

(deriv '(* x y) 'x)
