(import (rnrs)
        (base)
        (complex)
        (init)
        (functional)
        (generic-arithmetic)
        )

(init)
(display (make-complex-from-real-imag 2 3))
(newline)
(display (myreal-part (make-complex-from-real-imag 2 3)))
;How it works:
    ;after we call myreal-part, it calls apply-generic,
;which lookups key(op, args): (real-part, (complex)),
;which is myreal-part, then it recalls myreal-part,
;which calls apply-generic, and lookups key:(real-part, (rectangular)),
;it points to the func myreal-part in complex-rectangular.scm, and 2 is returned.
