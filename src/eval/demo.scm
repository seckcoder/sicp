(define (fib n)
  (display (let fib-iter ((a 1)
                 (b 0)
                 (count n))
    (if (= count 0)
      b
      (fib-iter (+ a b) a (- count 1))))
           )
  (display fib-iter)
  )


((lambda ()
   (define fib-iter
     (lambda (a b count)
       (if (= count 0)
         b
         (fib-iter (+ a b) a (- count 1))
         )
       ))
   (fib-iter 1 0 10)
   )
 )
