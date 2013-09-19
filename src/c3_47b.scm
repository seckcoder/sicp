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

; a not practical implementation of test-and-set!
(define (test-and-set! cell)
  (if (car cell)
    #t
    (begin (set-car! cell #t)
           #f)))

(define (clear! cell)
  (set-car! cell #f))

(define (semaphore-acquire sem)
  ((sem 'acquire)))

(define (semaphore-release sem)
  ((sem 'release)))

(define (test)
  (define sem (make-semaphore 2))
  (semaphore-acquire sem)
  (semaphore-acquire sem)
  (semaphore-release sem)
  (semaphore-release sem)
  (semaphore-acquire sem)
  )

(test)
