; eq? : takes two non-numeric atom.
; = : for numbers
; eqan?: atom equal
; eqlist?: list equal
; equal? : s-expression equal

; for real-world scheme: eq? is for reference equal
; while equal? is for value equal.

(define (atom? s)
  (and (not (pair? s))
       (not (null? s))))

; equivalent to guile's eq? 
(define (eqan? a1 a2)
  (cond ((and (number? a1) (number? a2))
         (= a1 a2))
        ; one is number while the other is not
        ((or (number? a1) (number? a2))
         #f)
        (else (eq? a1 a2))))

(define (eqlist? l1 l2)
  (cond ((and (null? l1) (null? l2)) #t)
        ((or (null? l1) (null? l2)) #f)
        (else
          (and (myequal? (car l1)
                         (car l2))
               (eqlist? (cdr l1)
                        (cdr l2))))))

(define (myequal? s1 s2)
  (cond ((and (atom? s1) (atom? s2))
         (eqan? s1 s2))
        ((or (atom? s1) (atom? s2)) #f)
        (else (eqlist? s1 s2))))


(define rember
  (lambda (s l)
    (cond ((null? l) '())
          ((myequal? s (car l))
           (cdr l))
          (else
            (cons (car l)
                  (rember s
                          (cdr l)))))))

(define insertL*
  (lambda (new old l)
    (cond ((null? l) '())
          ((myequal? old (car l))
           (cons new old (insertL* new old (cdr l))))
          ((list? (car l))
           (cons (insertL* new old (car l))
                 (insertL* new old (cdr l))))
          (else
            (cons (car l)
                  (insertL* new old (cdr l)))))))

(define (test-insertL*)
  (println (insertL* 'a 'b '(a b c))))


