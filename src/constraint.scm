;connector:
    ;has-value
    ;set-new-value!
    ;set-no-value
    ;connect

;add-constraint
    ;on-new-value
    ;on-no-value

;mul-constraint
    ;on-new-value
    ;on-no-value


(define (for-each-except excluded1 action included)
  (for-each (lambda (obj)
              (if (not (eq? obj excluded1))
                (action obj)))
            included))

(define (make-connector)
  (let ((value '())
        (constraints '()))
    (define (has-value)
      (null? value))

    (define (set-new-value! new-value informant)
      (cond ((not (has-value))
            (set! value new-value)
            (for-each-except informant
                             on-new-value
                       constraints))
            ((and (has-value)
                  (not (= value) (new-value)))
             (error 'set-new-value "CONFLICT NEWVALUE" new-value))
            (else
              'ignore)
            ))

    (define (connect constraint)
      (set! constraints (cons constraint constraints)))

    (define (dispatch m)
      (cond ((eq? m 'set-new-value) set-new-value!)
            ((eq? m 'connect) connect)
            ((eq? m 'has-value) (has-value))
            ((eq? m 'get-value) value)
            (else
              (error 'connector-dispatch "UNKNOWN MESSAGE" m))))
    dispatch)
  )

(define (connect connector constraint)
  ((connector 'connect) constraint))

(define (has-value connector)
  (connector 'has-value))

(define (get-value connector)
  (connector 'get-value))

(define (set-new-value! connector new-value informant)
  ((connector 'set-new-value) new-value informant))

(define (make-adder a1 a2 s)
  (define (on-new-value)
    (cond ((and (has-value a1)
                (has-value a2))
           (set-new-value! s (+ (get-value a1)
                               (get-value a2)) self))
          ((and (has-value a1)
                (has-value s))
           (set-new-value! a2 (- (get-value s)
                                (get-value a1)) self))
          ((and (has-value a2)
                (has-value s))
           (set-new-value! a1 (- (get-value s)
                                (get-value a2)) self))
          (else
            'ignore)))
  (define (self msg)
    (cond ((eq? msg 'on-new-value) (on-new-value))
          (else
            (error 'adder-self "UNKNOWN MESSAGE" msg))))

  (connect a1 self)
  (connect a2 self)
  (connect s self)
  self)

(define (make-multiplier m1 m2 p)
  (define (on-new-value)
    (cond ((or (and (has-value m1)
                    (= (get-value m1) 0))
               (and (has-value m2)
                    (= (get-value m2) 0)))
           (set-new-value! p 0 self))
          ((and (has-value m1)
                (has-value m2))
           (set-new-value! p (* (get-value m1)
                                (get-value m2)) self))
          ((and (has-value m1)
                (has-value p))
           (set-new-value! m2 (/ (get-value p)
                                 (get-value m1)) self))
          ((and (has-value m2)
                (has-value p))
           (set-new-value! m1 (/ (get-value p)
                                 (get-value m2)) self))
          (else
            'ignore)))
  (define (self msg)
    (cond ((eq? msg 'on-new-value) (on-new-value))
          (else
            (error 'multiplier-self "UNKNOWN MESSAGE" msg))))
  (connect m1 self)
  (connect m2 self)
  (connect p self)
  self)

(define (make-constant constant connector)
  (set-new-value! connector constant self)
  (define (self msg)
    (error 'constant-self "UNKNOWN MESSAGE" msg))
  self)

(define (on-new-value constraint)
  (constraint 'on-new-value))
