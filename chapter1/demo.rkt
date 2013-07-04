#lang racket


(require "c2_2.rkt")
(require "c2_3.rkt")

(define rectangle (make-rectangle (make-point 1.0 2.0)
                                  (make-point 2.0 1.0)))

(perimeter-rectangle rectangle)
(area-rectangle rectangle)
