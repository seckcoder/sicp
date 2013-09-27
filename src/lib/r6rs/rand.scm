(library
  (rand)
  (import (chezscheme))
  (export rand
          random-init
          random-update)
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
