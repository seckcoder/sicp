(import (rnrs)
        (base)
        (complex)
        (init)
        (functional)
        (generic-arithmetic)
        (dict)
        )

(init)
(define int (make-integer-number 3))
(define real (make-real-number 3.5))
(display (add int real))(newline)
