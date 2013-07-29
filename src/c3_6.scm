; This is not tested, since no rand-update func in chez-scheme...

(define (rand msg)
  (let ((x (random-init))
        (cond ((eq? msg 'generate)
               (begin
                 (set! x (rand-update x))
                 x))
              ((eq? msg 'reset)
               (lambda (new-init)
                 (set! x new-init)
                 x))
              (else
                (error 'rand "UNKNOWN MESSAGE" msg))))))
