; why we have y-combinator

; ok, some helper function
(define (add1 x)
  (+ x 1))

(define (eternity x)
  (eternity x))

; take length as example
; initial length definition
(define length
  (lambda (l)
    (cond ((null? l) 0)
          (else
            (add1 (length (cdr l)))))))

; how to implement it without use of define
; y-combinator

; length0
(lambda (l)
  (cond ((null? l) 0)
        (else
          (add1 (eternity (cdr l))))))

; length<=1
(lambda (l)
  (cond ((null? l) 0)
        (else
          (add1 ((lambda (l)
                   (cond ((null? l) 0)
                         (else
                           (add1 (eternity (cdr l)))))))))))

; abstract a function: mk-length
; mk-length: makes length from function that looks like length
(define mk-length
  (lambda (length)
    (lambda (l)
      (cond ((null? l) 0)
            (else
              (add1 (length (cdr l))))))))

; then, length0
((lambda (length)
   (lambda (l)
     (cond ((null? l) 0)
           (else
             (add1 (length (cdr l)))))))
 eternity)

; that is
(make-length eternity)

; then, length<=1
((lambda (length)
   (lambda (l)
     (cond ((null? l) 0)
           (else
             (add1 (length (cdr l)))))))
 ((lambda (length)
    (lambda (l)
      (cond ((null? l) 0)
            (else
              (add1 (length (cdr l)))))))
  eternity))

; that is
(make-length length0)

; = 
(make-length
  (make-length eternity))

; length<=2
((lambda (length)
   (lambda (l)
     (cond ((null? l) 0)
           (else
             (add1 (length (cdr l)))))))
 ((lambda (length)
    (lambda (l)
      (cond ((null? l) 0)
            (else
              (add1 (length (cdr l)))))))
  ((lambda (length)
     (lambda (l)
       (cond ((null? l) 0)
             (else
               (add1 (length (cdr l)))))))
   eternity)))

; that is
(make-length
  (make-length
    (make-length eternity)))

; make use of mk-length without define

; length0
((lambda (mk-length)
   (mk-length eternity))
 (lambda (length)
   (lambda (l)
     (cond ((null? l) 0)
           (else
             (add1 (length (cdr l))))))))

; length<=1
((lambda (mk-length)
   (mk-length
     (mk-length eternity)))
 (lambda (length)
   (lambda (l)
     (cond ((null? l) 0)
           (else
             (add1 (length (cdr l))))))))


; this can also be written as

((lambda (mk-length)
   (mk-length mk-length))
 (lambda (mk-length)
   (lambda (l)
     (cond ((null? l) 0)
           (else
             (add1 ((mk-length eternity)
                    (cdr l))))))))

; we don't care about the eternity func,
; it can be anything
((lambda (mk-length)
   (mk-length mk-length))
 (lambda (mk-length)
   (lambda (l)
     (cond ((null? l) 0)
           (else
             (add1 ((mk-length mk-length)
                    (cdr l))))))))

; we also want to use our mk-length func defintion.
; then we have
((lambda (mk-length)
   (mk-length mk-length))
 (lambda (mk-length)
   ; here is our mk-length
   ((lambda (length)
      (lambda (l)
        (cond ((null? l) 0)
              (else
                (add1 (length
                        (cdr l)))))))
    (mk-length mk-length))))


; try to expand it
((lambda (mk-length)
   ((lambda (length)
      (lambda (l)
        (cond ((null? l) 0)
              (else
                (add1 (length
                        (cdr l)))))))
    (mk-length mk-length)))
 (lambda (mk-length)
   ((lambda (length)
      (lambda (l)
        (cond ((null? l) 0)
              (else
                (add1 (length
                        (cdr l)))))))
    (mk-length mk-length))))

; again...
; Oh, shit! this is a infinite recursion
((lambda (length)
   (lambda (l)
     (cond ((null? l) 0)
           (else
             (add1 (length
                     (cdr l)))))))
 ; infinite recursion here when evaluate this.
 ; the problem is that, before we can evaluate the mk-length body
 ; wrap in lambda, we will first evaluate it's argument: (mk-length mk-length),
 ; which is recursive. therefore we need to make it lazy(evaluate when we really need
 ; it)
 ((lambda (mk-length)
    ((lambda (length)
       (lambda (l)
         (cond ((null? l) 0)
               (else
                 (add1 (length
                         (cdr l)))))))
     (mk-length mk-length)))
  (lambda (mk-length)
    ((lambda (length)
       (lambda (l)
         (cond ((null? l) 0)
               (else
                 (add1 (length
                         (cdr l)))))))
     (mk-length mk-length)))))

; make it lazy
(f x) = ((lambda (x)
           (f x))
         x)

((lambda (mk-length)
   (mk-length mk-length))
 (lambda (mk-length)
   (lambda (l)
     (cond ((null? l) 0)
           (else
             (add1 ((lambda (x)
                      ((mk-length mk-length) x))
                    (cdr l))))))))

; we want to use our mk-length func defintion
; then we have
((lambda (mk-length)
   (mk-length mk-length))
 (lambda (mk-length)
   ; here is our mk-length
   ((lambda (length)
      (lambda (l)
        (cond ((null? l) 0)
              (else
                (add1
                  ; lazy func will be evaluated here.
                  (length (cdr l)))))))
    ; here, the argument will not be evaluate until we call length
    (lambda (x)
      ((mk-length mk-length) x)))))

; how about extract mk-length definition, name it le
((lambda (le)
   ((lambda (mk-length)
      (mk-length mk-length))
    (lambda (mk-length)
      (le
        (lambda (x)
          ((mk-length mk-length) x))))))
 ; mk-length here
 (lambda (length)
   (lambda (l)
     (cond ((null? l) 0)
           (else
             (add1 (length (cdr l))))))))

; we define the part that is not related to length/mk-length as Y
; that is y-combinator
(define Y
  (lambda (le)
    ((lambda (f) (f f))
     (lambda (f)
       (le (lambda (x) ((f f) x)))))))

(define mk-length
  (lambda (length)
    (lambda (l)
      (cond ((null? l) 0)
            (else
              (add1 (length (cdr l))))))))

(Y mk-length) = length
