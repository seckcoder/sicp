; a) in eval.scm
; b) let's do some evaluation manually

; for letrec

(letrec ((even?
           (lambda (n)
             (if (= n 0)
               true
               (odd? (- n 1)))))
         (odd?
           (lambda (n)
             (if (= n 0)
               false
               (even? (- n 1))))))
  <exp>)

; ->

(let ((even? '*assigned*)
      (odd? '*assigned*))
  (set! even? ...)
  (set! odd? ...)
  <exp>)
; ->
((lambda (even? odd?)
   (set! even? ...)
   (set! odd? ...))
 '*assigned*
 '*assigned*
 )

; but for let

(let ((even? ...)
      (odd? ...))
  (even? x))

; ->

((lambda (even? odd?)
   (even? x))
 ; here, when evaluating the following lambda procedure,
 ; odd?, even? will be looked up in the lambda procedure environment(lexical scoping),
 ; therefore, they will not be found.
 (lambda (n)
   (if (= n 0)
     true
     (odd? (- n 1))))
 (lambda (n)
   (if (= n 0)
     false
     (even? (- n 1))))
)

; so, the internal diff between letrec and let is, letrec keeps the environment
; for argument in the let body.
