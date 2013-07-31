(import (constraint))

(define (print-connector c)
  (display (get-value c))(newline))

(define (test-cf-converter)
  (define C (make-connector))
  (define F (celsius-fahrenheit-converter C))
  (make-probe 'F F)
  (set-new-value! C 25 'user)
  (print-connector F)
  )

(test-cf-converter)
