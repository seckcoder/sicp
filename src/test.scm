(import (rnrs)
        (base)
        (complex)
        (init)
        )

(init)
(define a (make-from-real-imag 3 0.5))
(myreal-part a)
