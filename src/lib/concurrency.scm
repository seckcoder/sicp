(library
  (concurrency)
  (export parallel-execute
          make-semaphore
          semaphore-acquire
          semaphore-release)

  (import (chezscheme))

  (define (parallel-execute . args)
    (for-each fork-thread args))

  (define (make-serializer)
    (let ((mutex (make-mutex)))
      (lambda (p)
        (define (serialized-p . args)
          (mutex 'acquire)
          (let ((val (apply p args)))
            (mutex 'release)
            val))
        serialized-p)))
  (define (make-semaphore n)
    (let ((mutex (make-mutex)))
      (define (acquire . args)
        (let ((block? (if (null? args)
                        #t
                        (car args))))
          ; Note n's if test should be protected by mutex.
          (mutex-acquire mutex)
          (if (> n 0)
            (begin (set! n (- n 1))
                   (mutex-release mutex)
                   #t)
            (begin (mutex-release mutex)
                   (if block?
                     (acquire)
                     #f)))
          ))
      (define (release)
        (mutex-acquire mutex)
        (set! n (+ n 1))
        (mutex-release mutex))

      (define (self msg)
        (cond ((eq? msg 'acquire) acquire)
              ((eq? msg 'release) release)
              (else
                (error 'semaphore-self "UNKNOWN MESSAGE" msg))))
      self))

  (define (semaphore-acquire sem . args)
    (apply (sem 'acquire) args))

  (define (semaphore-release sem)
    (sem 'release))
  )
