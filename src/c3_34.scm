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
      (set! constraints (cons constraint constraints)))

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
  (set-new-value! connector constant self)
  (connect connector self)
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



(define (make-squarer a b)
  (make-multiplier a a b))

(define (test-squarer)
  (let ((a (make-connector))
        (b (make-connector)))
    (make-squarer a b)
    (make-probe 'a a)
    (set-new-value! b 9 'user) ; we cannot get a from b
    ; besides, when we set a, the on-new-value procedure will be called twice.
    ; But this seems will not cause any serious problem...
    ))

(test-squarer)
