(define (mystery x)
  (define (loop x y)
    (display x)(newline)
    (display y)(newline)
    (if (null? x)
      y
      (let ((temp (cdr x)))
        (set-cdr! x y)
        (display x)(newline)
        (loop temp x))))
  (loop x '()))

;mystery reverse the list x

; 代码写成这个狗屎样是想死啊!!!
(define (reverse! x)
  (define (move-car x y)
    ; concates (car x) and y, and do the same for (cdr x), x
    (if (null? x)
      y
      (let ((rest (cdr x)))
        (set-cdr! x y)
        (move-car rest x))))
  (move-car x '()))

(display (reverse! '(1 2 3)))
