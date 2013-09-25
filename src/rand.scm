; rand.scm for mit-scheme
(define random-init (get-universal-time))
(define (rand-update x)
  (let ((a 40)
        (b 2641)
        (m 729))
    (modulo (+ (* a x) b) m)))

(define rand
  (let ((x random-init))
    (lambda ()
      (set! x (rand-update x))
      x)))

(display (rand))
