(import (generic-arithmetic)
        (init))

(init)
(display (equ? (make-scheme-number 2) (make-scheme-number 3)))(newline)
(display (equ? (make-scheme-number 2) (make-scheme-number 2)))(newline)
(display (equ? (make-rational-number 2 3) (make-rational-number 4 5)))(newline)
(display (equ? (make-rational-number 2 3) (make-rational-number 4 6)))(newline)
(display (equ? (make-complex-from-real-imag 2 3) (make-complex-from-real-imag 2 4)))(newline)
(display (equ? (make-complex-from-real-imag 2 3) (make-complex-from-real-imag 2 3)))(newline)
