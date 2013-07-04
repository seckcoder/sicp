#lang racket

(require racket/block)
(require "base.rkt")

;; I have no interest in looking into the buggy code, so I wrote
;; a iterative map instead.
;; the iterative map
(define (map-iter proc lst)
  (define (iter input output)
    (if (null? input)
      output
      (block
        (iter (cdr input)
              (cons (proc (car input))
                    output)))))
  (reverse (iter lst null)))

(map-iter square '(1 2 3 4))
(map-iter square null)
