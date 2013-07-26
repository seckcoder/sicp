(library
  (coercion)
  (export install-coercion
          number->polynomial)
  (import (rnrs)
          (base)
          (generic-arithmetic)
          (symbolic-algebra)
          (table2d))

  ; we need some information of the polynomial the number that's going
  ; to be converted into
  (define (number->polynomial number poly . rest)
    (make-polynomial (variable (contents poly))
                     (list (make-term 0 number))))
  (define (install-coercion)
    (put-coercion 'complex 'polynomial number->polynomial)
    )
  )
