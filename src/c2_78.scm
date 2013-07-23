; this script should be executed in branch c2_78
(import (rnrs)
        (base)
        (init)
        (generic-arithmetic))

(init)
(display (make-scheme-number 3))(newline)
(display (add (make-scheme-number 3)
              (make-scheme-number 4)))(newline)
