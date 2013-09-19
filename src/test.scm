(define (make-semaphore n)
  (let ((cell (list #f)))
    (define (acquire)
      (if (test-and-set! cell)
        (acquire)
        (begin
          (cond ((> n 0)
                 (set! n (- n 1))
                 (clear! cell)
                 #t)
                (else
                  (clear! cell)
                  (acquire))))))
    (define (release)
      (if (test-and-set! cell)
        (begin
          (clear! cell)
          (release))
        (begin
          (set! n (+ n 1))
          (clear! cell))))
    (define (self msg)
      (cond ((eq? msg 'acquire) acquire)
            ((eq? msg 'release) release)
            (else
              (error 'semaphore-self "UNKNOWN MESSAGE" msg))))
    self))
