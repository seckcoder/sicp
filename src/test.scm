(import (rnrs)
        (base)
        (complex)
        (init)
        (functional)
        (generic-arithmetic)
        (symbolic-algebra)
        (coercion)
        (dict)
        (poly-terms)
        )

(init)


(beautiful-display-terms 'x (make-termlist 'dense (list (make-term 3
                                                                            (make-integer-number 2))
                                                                 (make-term 1
                                                                            (make-integer-number 1)))))
