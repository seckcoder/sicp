(library
  (chez)
  (export sleepfor
          parallel-execute
          make-semaphore
          semaphore-acquire
          semaphore-release
          rand
          random-in-range
          random-init
          random-update
          )
  (import (chezscheme))

  ; @(utils)
  ; sleep for s seconds
  (define (sleepfor s)
    (sleep (make-time 'time-duration 0 s)))

  ; @(concurrency)
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

  ; a not practical implementation of test-and-set!
  (define (test-and-set! cell)
    (if (car cell)
      #t
      (begin (set-car! cell #t)
             #f)))

  (define (clear! cell)
    (set-car! cell #f))

  (define (mutex-test-and-set-impl)
    (define (make-mutex)
      (let ((cell (list #f)))
        (define (self msg)
          (cond ((eq? msg 'acquire)
                 (if (test-and-set! cell)
                   (self 'acquire)
                   #t))
                ((eq? msg 'release)
                 (clear! cell))))
        self)))

  ; [low high)
  (define (random-in-range low high)
    (let ((range (- high low)))
      (+ low (random range))))
  (define random-init (time-second (current-time)))
  (define (random-update x)
    (let ((a 40)
          (b 2641)
          (m 729))
      (modulo (+ (* a x) b) m)))

  (define rand
    (let ((x random-init))
      (lambda ()
        (set! x (rand-update x))
        x)))
  )
