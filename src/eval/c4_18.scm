; There is a series problem of the scanned out method shown in the exercise.
(lambda ()
  (let ((u '*unassigned*)
        (v '*unassigned*))
    ; Here, when some-expression is evaluated, if they contain circular dependance
    ; like solve has, u/v may be evaluated, in which case an error reported.
    (let ((a 'some-expression)
          (b 'some-expression))
      (set! u a)
      (set! v b))
    'expressions))


; here shows the case

(define (solve f y0 dt)
  (define y (integral (delay dy) y0 dt))
  (define dy (stream-map f y))
  y)

; ->

(define solve
  (lambda (f y0 dt)
    (let ((y '*assigned*)
          (dy '*assigned*))
      (let ((a (integral (delay dy) y0 dt))
            (b (stream-map f y)))
        (set! y a)
        (set! dy b)
        y))))

; ->
(define solve
  (lambda (f y0 dt)
    ((lambda (y dy)
       (let ((a (integral (delay dy) y0 dt))
             (b (stream-map f y)))
         (set! y a)
         (set! dy b)
         y))
     '*assigned*
     '*assigned*)))
; ->
(define solve
  (lambda (f y0 dt)
    ((lambda (y dy)
       ((lambda (a b)
          (set! y a)
          (set! dy b)
          y)
        ; here, procedure args are evaluated, and error reported.
        (integral (delay dy) y0 dt)
        (stream-map f y))
       '*assigned*
       '*assigned*))
    ))


; apparently, there is no such problem for the one in the text
