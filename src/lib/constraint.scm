(library
  (constraint)
  (export make-connector
          make-adder
          make-multiplier
          make-constant
          make-probe
          set-new-value!
          get-value)
  (import (rnrs)
          (rnrs mutable-pairs)
          )

  (define (for-each-except excluded1 action included)
    (for-each (lambda (obj)
                (if (not (eq? obj excluded1))
                  (action obj)))
              included))

  (define (make-connector)
    (let ((value '())
          (constraints '())
          (informant '()))
      (define (has-value)
        (not (null? value)))

      (define (set-new-value! new-value setter)
        (cond ((not (has-value))
               (set! value new-value)
               (set! informant setter)
               (for-each-except setter
                                on-new-value
                                constraints))
              ((not (= value new-value))
               (error 'set-new-value "CONFLICT NEWVALUE" new-value))
              (else
                'ignore)
              ))

      (define (set-lost-value! retractor)
        ; only accept request from the original informant
        (if (eq? retractor informant)
          (begin (set! informant '())
                 (for-each-except retractor
                                  on-lost-value
                                  constraints))
          'ignored))

      (define (connect constraint)
        (if (not (memq constraint constraints))
          (set! constraints (cons constraint constraints)))
        ; if the contraint already has value, then we
        ; call it's on-new-value.
        ; For the case: (c* (cv 6) (cv 5)).
        ; When call c*, connect to (cv 6) or (cv 5), the on-new-value
        ; will be called.
        (if (has-value)
          (on-new-value constraint))
        'done)

      (define (dispatch m)
        (cond ((eq? m 'set-new-value) set-new-value!)
              ((eq? m 'set-lost-value) set-lost-value!)
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

  (define (set-lost-value! connector informant)
    ((connector 'set-lost-value) informant))

  ; Note those makers here is a little special. We actually don't
  ; need to return self(since we alread connect it with a1/a2/s).
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

    (define (on-lost-value)
      (set-lost-value! s self)
      (set-lost-value! a2 self)
      (set-lost-value! a1 self)
      (on-new-value))
    (define (self msg)
      (cond ((eq? msg 'on-new-value) (on-new-value))
            ((eq? msg 'on-lost-value) (on-lost-value))
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
    (define (on-lost-value)
      (set-lost-value! p self)
      (set-lost-value! m2 self)
      (set-lost-value! m1 self)
      (on-new-value))

    (define (self msg)
      (cond ((eq? msg 'on-new-value) (on-new-value))
            ((eq? msg 'on-lost-value) (on-lost-value))
            (else
              (error 'multiplier-self "UNKNOWN MESSAGE" msg))))
    (connect m1 self)
    (connect m2 self)
    (connect p self)
    self)

  (define (make-constant constant connector)
    (define (self msg)
      (error 'constant-self "UNKNOWN MESSAGE" msg))
    ; Note the order here is important. Otherwise,
    ; there maybe bugs: (c* (cv 4) (cv 3))
    ; Here, c*'s on-new-value will not be called if the order is reversed.
    (connect connector self)
    (set-new-value! connector constant self)
    self)

  (define (make-probe name connector)
    (define (on-new-value)
      (display name)
      (display " Got value :")
      (display (get-value connector))
      (newline)
      )
    (define (on-lost-value)
      (display name)
      (display " lost value")
      (newline)
      )
    (define (self msg)
      (cond ((eq? msg 'on-new-value) (on-new-value))
            ((eq? msg 'on-lost-value) (on-lost-value))
            (else
              (error 'probe-self "UNKNOWN MESSAGE" msg))))
    (connect connector self)
    self)

  (define (on-new-value constraint)
    (constraint 'on-new-value))

  (define (on-lost-value constraint)
    (constraint 'on-lost-value))


  (define (celsius-fahrenheit-converter c f)
    (let ((u (make-connector))
          (v (make-connector))
          (w (make-connector))
          (x (make-connector))
          (y (make-connector)))
      (make-multiplier c w u)
      (make-multiplier v x u)
      (make-adder v y f)
      (make-constant 9 w)
      (make-constant 5 x)
      (make-constant 32 y)
      'ok))


  (define (test-cf-converter)
    (define C (make-connector))
    (define F (make-connector))
    (celsius-fahrenheit-converter C F)
    (make-probe "Celsius temp" C)
    (make-probe "Fahrenheit temp" F)
    (set-new-value! C 25 'user)
    (set-lost-value! C 'user)
    )
  )