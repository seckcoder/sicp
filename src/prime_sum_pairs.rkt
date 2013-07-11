#lang racket
(require "base.rkt")
(require "prime.rkt")
(require "functional.rkt")

(define (unique-pairs n)
  (flatmap (lambda (i)
         (map (lambda (j) (list i j))
              (enumerate-interval 1 i)))
       (enumerate-interval 2 n)))

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (prime-pair-sum? pair)
  (prime? (+ (car pair)
             (cadr pair))))

#|(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-pair-sum?
               (flatmap (lambda (i)
                          (map (lambda (j)
                                 (list i j))
                               (enumerate-interval 1 (- i 1))))
                        (enumerate-interval 1 n)))))
|#

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-pair-sum?
               (unique-pairs n))))

(define (permutations s)
  (if (null? s)
    (list null)
    ;; why flatmap? After every map, there will be an extra level bracket,
    ;; which have to be erased by flatmap
    (flatmap (lambda (x)
               (map (lambda (p) (cons x p))
                    (permutations (remove x s))))
             s)))
; See the following demo to see why we need flatmap
#|(define (permutations s)
  (if (null? s)
    (list null)
    (flatmap (lambda (x)
               (let ((lst (permutations (remove x s))))
                 (let ((tmp (map (lambda (p)
                                   (cons x p))
                                 (permutations (remove x s)))))
                   ;note tmp is what we want(the same as permutations),
                   ; however, the outter map will wrap another brackets around tmp,
                   ; therefore, we have to erase it(use flatmap)
                   (display tmp)  
                   (display "*** ")
                   (display lst)
                   (newline)
                   tmp)))
             s)))|#
(prime-sum-pairs 10)
;(permutations '(1 2 3))
