(import (constraint))

(define (make-squarer a b)
  (make-multiplier a a b))

(define (test-squarer)
  (let ((a (make-connector))
        (b (make-connector)))
    (make-squarer a b)
    (make-probe 'a a)
    ; we cannot get a from b. different from multiplier, we can know a when we
    ; only know b.
    (set-new-value! b 9 'user)
    ; besides, when we set a, the on-new-value procedure will be called twice.
    ; But this seems will not cause any serious problem...
    ))

(test-squarer)
