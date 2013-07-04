#lang racket

(require "base.rkt")
(require "c2_2.rkt")
(define (make-rectangle lower-left upper-right)
  (cons lower-left upper-right))

(define (lower-left-rectangle rectangle)
  (car rectangle))

(define (upper-right-rectangle rectangle)
  (cdr rectangle))

(define (upper-left-rectangle rectangle)
  (make-point (x-point (lower-left-rectangle rectangle))
              (y-point (upper-right-rectangle rectangle))))

(define (lower-right-rectangle rectangle)
  (make-point (x-point (upper-right-rectangle rectangle))
              (y-point (lower-left-rectangle rectangle))))

(define (sideslen-rectangle rectangle)
  (cons (length-points (lower-left-rectangle rectangle)
                                 (lower-right-rectangle rectangle))
        (length-points (lower-left-rectangle rectangle)
                                 (upper-left-rectangle rectangle))))

(define (perimeter-rectangle rectangle)
  (let ((sideslen (sideslen-rectangle rectangle)))
    (double (+ (car sideslen)
               (cdr sideslen)))))

(define (area-rectangle rectangle)
  (let ((sideslen (sideslen-rectangle rectangle)))
    (* (car sideslen)
       (cdr sideslen))))

(provide (all-defined-out))
