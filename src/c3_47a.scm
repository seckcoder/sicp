; in lib/concurrency.scm. The implementation is not tested in
; complicated situation.

(import (concurrency)
        (functional)
        (utils))

(define sem (make-semaphore 2))

(display (apply-n semaphore-acquire (list sem #f) 3))
