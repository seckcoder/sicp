(library
  (init)
  (export init)
  (import (rnrs)
          (complex-rectangular)
          (complex-polar)
          (base)
          (deriv)
          (generic-arithmetic)
          (symbolic-algebra)
          (coercion)
          (poly-terms))
  (define (init)
    (install-rectangular-package)
    (install-polar-package)
    (install-deriv-package)
    (install-integer-package)
    (install-real-package)
    (install-rational-package)
    (install-complex-package)
    (install-polynomial-package)
    ;(install-coercion)
    (install-sparse-poly-terms-package)
    (install-dense-poly-terms-spackage)
    (build-tower!)
    )
  )
