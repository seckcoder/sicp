(import (queue))

(define (digital-circuit)
  (define (inverter input output)
    (define (invert-input)
      (let ((new-value (logical-not (get-signal input))))
        (after-delay inverter-delay
                     (lambda ()
                       (display "invert-gate-callback")(newline)
                       (set-signal! output new-value)))))
    (add-action! input invert-input)
    'ok)

  (define (logical-not s)
    (cond ((= s 0) 1)
          ((= s 1) 0)
          (else (error 'logical-not "Invalid signal" s))))

  (define (and-gate a1 a2 output)
    (define (and-action-procedure)
      (let ((new-value (logical-and (get-signal a1)
                                    (get-signal a2))))
        (after-delay and-gate-delay
                     (lambda ()
                       (display "and-gate-callback")(newline)
                       (set-signal! output new-value)))))
    (add-action! a1 and-action-procedure)
    (add-action! a2 and-action-procedure)
    'ok)

  (define (logical-and s1 s2)
    (cond ((and (= s1 1) (= s2 1)) 1)
          (else 0)))

  (define (or-gate a1 a2 output)
    (define (or-action-procedure)
      (let ((new-value (logical-or (get-signal a1)
                                   (get-signal a2))))
        (after-delay or-gate-delay
                     (lambda ()
                       (display "or-gate-callback")(newline)
                       (set-signal! output new-value)))))
    (add-action! a1 or-action-procedure)
    (add-action! a2 or-action-procedure)
    'ok)


  (define (logical-or s1 s2)
    (cond ((or (= s1 1) (= s2 1)) 1)
          (else 0)))

  (define (call-each procedures)
    (if (null? procedures)
      'done
      (begin
        ((car procedures))
        (call-each (cdr procedures)))))

  (define (make-wire)
    (let ((signal-value 0) (action-procedures '()))
      (define (set-my-signal! new-value)
        (if (not (= signal-value new-value))
          (begin (set! signal-value new-value)
                 (call-each action-procedures))))
      (define (accept-action-procedure! proc)
        (set! action-procedures (cons proc action-procedures))
        (proc))
      (define (dispatch m)
        (cond ((eq? m 'get-signal) signal-value)
              ((eq? m 'set-signal!) set-my-signal!)
              ((eq? m 'add-action!) accept-action-procedure!)
              (else (error 'make-wire-dispatch "UNKNOWN MESSAGE" m))))
      dispatch))

  (define (get-signal wire)
    (wire 'get-signal))

  (define (set-signal! wire new-value)
    ((wire 'set-signal!) new-value))

  ; add a procedure to wire
  (define (add-action! wire action-procedure)
    ((wire 'add-action!) action-procedure))

  ; add a procedure to agenda
  (define (after-delay delay action)
    (add-to-agenda! (+ delay (current-time the-agenda))
                    action
                    the-agenda))

  (define (propagate)
    (if (empty-agenda? the-agenda)
      'done
      (let ((first-item (first-agenda-item the-agenda)))
        (first-item)
        (remove-first-agenda-item! the-agenda)
        (propagate))))

  ; add an action for the wire to display wire information when signal changed
  (define (probe name wire)
    (add-action! wire
                 (lambda ()
                   (newline)
                   (display name)
                   (display " ")
                   (display (current-time the-agenda))
                   (display " NEW-VALUE = ")
                   (display (get-signal wire))
                   (newline))))

  (define (make-agenda) (list 0))
  (define the-agenda (make-agenda))
  (define inverter-delay 2)
  (define and-gate-delay 3)
  (define or-gate-delay 5)
  (define (current-time agenda)
    (car agenda))
  (define (set-current-time! agenda time)
    (set-car! agenda time))
  (define (segments agenda) (cdr agenda))
  (define (set-segments! agenda segments)
    (set-cdr! agenda segments))
  (define (first-segment agenda) (car (segments agenda)))
  (define (rest-segments agenda) (cdr (segments agenda)))
  (define (empty-agenda? agenda)
    (null? (segments agenda)))
  (define (make-time-segment time queue)
    (cons time queue))
  (define (segment-time s) (car s))
  (define (segment-queue s) (cdr s))
  (define (add-to-agenda! time action agenda)
    ; whether we can insert the segment in the head of agenda
    (define (belongs-before? segments)
      (or (null? segments)
          (< time (segment-time (car segments)))))
    (define (make-new-time-segment time action)
      (let ((q (make-queue)))
        (insert-queue! q action)
        (make-time-segment time q)))
    (define (add-to-segments! segments)
      (if (= (segment-time (car segments)) time)
        (insert-queue! (segment-queue (car segments))
                       action)
        (let ((rest (cdr segments)))
          (if (belongs-before? rest)
            (set-cdr!
              segments
              (cons (make-new-time-segment time action)
                    (cdr segments)))))))
    (let ((segments (segments agenda)))
      (if (belongs-before? segments)
        (set-segments!
          agenda
          (cons (make-new-time-segment time action)
                segments))
        (add-to-segments! segments))))

  (define (remove-first-agenda-item! agenda)
    (let ((q (segment-queue (first-segment agenda))))
      (delete-queue! q)
      (if (empty-queue? q)
        (set-segments! agenda (rest-segments agenda)))))

  (define (first-agenda-item agenda)
    (if (empty-agenda? agenda)
      (error 'first-agenda-item "Agenda is empty")
      (let ((first-seg (first-segment agenda)))
        (set-current-time! agenda (segment-time first-seg))
        (front-queue (segment-queue first-seg)))))

  (define (half-adder a b s c)
    (let ((d (make-wire))
          (e (make-wire)))
      (or-gate a b d) ; delay 5
      (and-gate a b c) ; delay 3
      (inverter c e) ; delay 2
      (and-gate d e s)) ; delay 3
    'ok)

  (define (test-1)
    (define input-1 (make-wire))
    (define input-2 (make-wire))
    (define sum (make-wire))
    (define carry (make-wire))
    (probe 'sum sum)(newline)
    (probe 'carry carry)(newline)
    (half-adder input-1 input-2 sum carry)
    (propagate)
    (display (get-signal carry))
    )

  (define (test-2)
    (define input (make-wire))
    (define output (make-wire))
    (inverter input output)
    (probe 'output output)
    (propagate)
    (display (get-signal output))
    )
  (test-1)
  )

(digital-circuit)
