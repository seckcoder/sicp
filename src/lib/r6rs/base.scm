; type system. For historical reason, this file is named base.
(library
  (base)
  (export operation-table get get-or-fail put
          coercion-table get-coercion put-coercion
          type-tag contents attach-tag apply-generic myraise
          square
          declare-type build-tower!
          add sub mul negate divide equ? =zero? dropif beautiful-display
          isinteger? equal-int1?
          )
  (import (rnrs)
          (rnrs mutable-pairs)
          (table2d)
          (utils)
          (dict)
          )
  ; operation table
  (define operation-table (make-table))
  (define get (operation-table 'lookup-proc))
  (define (get-or-fail . args)
    (let ((proc (apply get args)))
      (if proc
        proc
        (error 'get-or-fail "CANNOT FIND" args))))
  (define put (operation-table 'insert-proc!))

  ; coercion table
  (define coercion-table (make-table))
  (define get-coercion (coercion-table 'lookup-proc))
  (define put-coercion (coercion-table 'insert-proc!))

  (define (attach-tag type-tag contents)
    (cons type-tag contents))
  (define (type-tag datum)
    (if (pair? datum)
      (car datum)
      (error 'type-tag "Bad tagged datum" datum)))
  (define (contents datum)
    (if (pair? datum)
      (cdr datum)
      (error 'contents "Bad tagged datum" datum)))

  (define type-tower (make-dict))
  (define (type-tower-insert! k v)
    ((type-tower 'insert) k v))
  (define (type-tower-lookup k)
    ((type-tower 'lookup) k))
  (define (type-tower-lookup-or-fail k)
    (let ((type-attr (type-tower-lookup k)))
      (if (not (null? type-attr))
        type-attr
        (error 'type-tower-lookup-or-fail "CANNOT FIND:" k)
        ))
    )

  ; randomly return a type
  (define (type-tower-atype)
    ((type-tower 'akey)))

  (define (typed? x)
    (if (not (pair? x))
      #f
      (let ((type (type-tag x)))
        (inlist? x
                 (list 'integer 'real 'rational 'complex)
                 eq?))))

  (define (put-type type base child precedence)
    (type-tower-insert! type (make-type-attr base child precedence)))
  (define (declare-type type base)
    (let ((type-attr (type-tower-lookup type))
          (base-attr (type-tower-lookup base)))
      (if (null? type-attr)
        (put-type type base '() -1)
        (set-type-attr-base! type-attr base))
      (if (null? base-attr)
        (put-type base '() type -1)
        (set-type-attr-child! base-attr type))
      #t)
    )

  (define (set-type-precedence! type precedence)
    (let ((type-attr (type-tower-lookup-or-fail type)))
      (set-type-attr-precedence! type-attr precedence)))
  (define (get-type-base type)
    (type-attr-base (type-tower-lookup-or-fail type)))
  (define (get-type-precendence type)
    (type-attr-precedence (type-tower-lookup-or-fail type)))
  (define (get-type-child type)
    (type-attr-child (type-tower-lookup-or-fail type)))
  (define (type-attr-base type-attr) (car type-attr))
  (define (set-type-attr-base! type-attr base) (set-car! type-attr base))
  (define (type-attr-child type-attr) (cadr type-attr))
  (define (set-type-attr-child! type-attr child) (set-cadr! type-attr child))
  (define (type-attr-precedence type-attr) (caddr type-attr))
  (define (set-type-attr-precedence! type-attr precedence)
    (set-caddr! type-attr precedence))
  (define (make-type-attr base child precedence)
    (list base child precedence))
  (define (type-top? type)
    (null? (get-type-base type)))
  (define (type-bottom? type)
    (null? (get-type-child type)))

  (define (type> type1 type2)
    (cond ((and (type-top? type1)
                (type-top? type2)) #f)
          ((type-top? type1) #t)
          ((type-top? type2) #f)
          ((> (get-type-precendence type1)
              (get-type-precendence type2))
           #t)
          (else #f)))

  (define (type= type1 type2)
    (cond ((and (type-top? type1)
                (type-top? type2)) #t)
          ((or (type-top? type1)
               (type-top? type2)) #f)
          (else (= (get-type-precendence type1)
                   (get-type-precendence type2)))))

  (define (type< type1 type2)
    (and (not (type> type1 type2))
         (not (type= type1 type2))))

  (define (type-tower-lowest)
    (let ((type (type-tower-atype)))
      (define (recur type)
        (cond ((type-bottom? type) type)
              (else (recur (get-type-child type)))))
      (recur type)))

  (define (update-type-precedence-bottom-up type precedence)
    (cond ((type-top? type) #t)
          (else (begin
                  (set-type-precedence! type precedence)
                  (update-type-precedence-bottom-up (get-type-base type)
                                                    (+ 1 precedence))))))

  (define (build-tower!)
    (let ((lowest (type-tower-lowest)))
      (update-type-precedence-bottom-up lowest 0)))

  (define (apply-generic-type-tower op . args)
    ; args corresponding to the args of op
    (let ((type-tags (map type-tag args)))
      (define (error-msg)
        (error
          'apply-generic-type-tower
          "No method for these types"
          (list op type-tags)))
      (let ((proc (get op type-tags)))
        (if proc
          (apply proc (map contents args))
          (if (= (length type-tags) 2)
            (let ((type1 (car type-tags))
                  (arg1 (car args))
                  (type2 (cadr type-tags))
                  (arg2 (cadr args)))
              ; first try coercion
              (let ((t1->t2 (get-coercion type1 type2))
                    (t2->t1 (get-coercion type2 type1)))
                (cond (t1->t2
                        (apply-generic op
                                       (t1->t2 arg1 arg2)
                                       arg2))
                      (t2->t1
                        (apply-generic op arg1 (t2->t1 arg2 arg1)))
                      (else 
                        ; then try raise
                        (cond ((and (type-top? type1) (type-top? type2))
                               ; reached top, give up
                               (error-msg))
                              ((type> type1 type2)
                               (apply-generic-type-tower op arg1 (myraise arg2)))
                              ((type= type1 type2)
                               (apply-generic-type-tower op
                                                         (myraise arg1)
                                                         (myraise arg2)))
                              ((type< type1 type2)
                               (apply-generic-type-tower op (myraise arg1) arg2)))
                        ))
                )
              )
            (error-msg))
          )
        )))
  (define (myraise x) 
    ((get-or-fail 'raise (type-tag x)) (contents x)))
  (define (project x) ((get-or-fail 'project (type-tag x)) (contents x)))
  (define (add x y) (apply-generic 'add x y))
  (define (sub x y) (apply-generic 'sub x y))
  (define (mul x y) (apply-generic 'mul x y))
  (define (divide x y) (apply-generic 'div x y))
  (define (negate x) ((get-or-fail 'negate (type-tag x)) (contents x)))
  (define (equ? x y) (apply-generic 'equ? x y))
  (define (make-zero type-tag) (get 'zero type-tag))
  (define (beautiful-display x) ((get-or-fail 'display (type-tag x)) (contents x)))
  (define (=zero? x)
    (let ((equal-zero-proc (get '=zero? (type-tag x))))
      ; if the type has defined =zero?, we will use it.
      ; otherwise we use the default(equal to zero)
      (if equal-zero-proc
        (equal-zero-proc (contents x))
        (equ? x (make-zero (type-tag x))))))

  (define (isinteger? x)
    (eq? (type-tag x) 'integer))

  (define (equal-int1? x)
    (let ((dropped-x (dropif x)))
      (and (isinteger? x)
           (= (contents x) 1))))

  (define (dropif x)
    (if (get 'project (type-tag x))
      (let ((simplified-x (project x)))
        (if (get 'raise (type-tag simplified-x))
          (let ((raised (myraise simplified-x)))
            (if (equ? raised x)
              (dropif simplified-x)
              x))
          x))
      x))
  (define (apply-generic op . args)
    (let ((v (apply apply-generic-type-tower (cons op args))))
      (if (typed? v)
        (dropif v)
        v)))
  )
