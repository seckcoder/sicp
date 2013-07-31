; in lib/constraint.scm


(import (constraint))



(print-connector (csquare (cv 3)))

(let ((a (make-connector)))
  (define c (csquare a))
  (set-new-value! c 4 'user)
  (print-connector a))
