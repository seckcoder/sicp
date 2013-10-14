(define (atom? x)
  (and (not (pair? x)) (not (null? x))))

; pair is consisted of two s-expressions
; different from pair?
(define (a-pair? x)
  (cond ((atom? x) #f)
        ((null? x) #f)
        ((null? (cdr x)) #f)
        ((null? (cdr (cdr x))) #t)
        (else #f)))

(define looking
  (lambda (a lat)
    (keep-looking a (pick 1 lat) lat)))

(define (pick n lat)
  (cond ((and (= n 1)
             (not (null? lat)))
         (car lat))
        ((= n 1)
         (error 'pick "lat is null"))
        (else
          (pick (- n 1)
                (cdr lat)))))

(define (keep-looking a sorn lat)
  (cond ((number? sorn)
         (keep-looking a (pick sorn lat) lat))
        (else
          (eq? a sorn))))
          
; build s1 and s2 into a list
(define (build s1 s2)
  (cons s1 (cons s2 '())))

(define (first pair)
  (car pair))

(define (second pair)
  (cadr pair))

(define (shift pair)
  (build (first (first pair))
         (build (second (first pair))
                (second pair))))

; pora is a special pair of an atom. The special pair is a pair
; consisted of two special pair or atom.
; Example:
;   '((1 2) ((3 4) (5 6)))
;   align it
;       '(1 (2 (3 (4 (5 6)))))
(define (align pora)
  (cond ((atom? pora) pora)
        ((a-pair? (first pora))
         (align (shift pora)))
        (else (build (first pora)
                     (align (second pora))))))

(define (weight* pora)
  (cond ((atom? pora) 1)
        (else
          (+ (* (weight* (first pora)) 2)
             (weight* (second pora))))))


; reverse a pair
(define (revpair p)
  (build (second p) (first p)))

(define (shuffle pora)
  (cond ((atom? pora) pora)
        ((a-pair? (first pora))
         (shuffle (revpair pora)))
        (else (build (first pora)
                     (shuffle (second pora))))))
