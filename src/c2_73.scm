;a): the number of arguments is not the same.
;b)

(import (base)
        (init)
        (deriv)
        )

(init)
(display (deriv '(+ x 3) 'x))
; the details are in lib/deriv.scm

; c)
; we just need to change install-deriv-package
