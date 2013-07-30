(import (utils))

(define (make-deque)
  (let ((front-ptr '())
        (rear-ptr '()))
    (define (make-q) (cons front-ptr rear-ptr))
    (define (empty-deque?) (or (null? front-ptr)
                               (null? rear-ptr)))
    (define (content ptr) (car ptr))
    (define (next ptr) (cadr ptr))
    (define (previous ptr) (caddr ptr))
    (define (set-next! ptr new) (set-cadr! ptr new))
    (define (set-previous! ptr new) (set-caddr! ptr new))
    (define (make-ptr item next prev) (list item next prev))
    (define (set-front-ptr! ptr) (set! front-ptr ptr))
    (define (set-rear-ptr! ptr) (set! rear-ptr ptr))
    (define (front-deque)
      (if (empty-deque?)
        (error 'front-deque "DEQUE IS EMPTY")
        (content front-ptr)))
    (define (rear-deque)
      (if (empty-deque?)
        (error 'rear-deque "DEQUE IS EMPTY")
        (content rear-ptr)))
    (define (front-insert-deque! item)
      (let ((ptr (make-ptr item '() '())))
        (cond ((empty-deque?)
               (set-front-ptr! ptr)
               (set-rear-ptr! ptr)
               (make-q))
              (else
                (set-next! ptr front-ptr)
                (set-previous! front-ptr ptr)
                (set-front-ptr! ptr)
                (make-q)))))

    (define (front-delete-deque!)
      (cond ((empty-deque?)
             (error 'front-delete-deque! "DELETE EMPTY DEQUE"))
            (else
              (let ((new-front-ptr (next front-ptr)))
                (if (not (null? new-front-ptr))
                  (set-previous! new-front-ptr '()))
                (set-front-ptr! new-front-ptr)
                (make-q)))))

    (define (rear-insert-deque! item)
      (let ((ptr (make-ptr item '() '())))
        (cond ((empty-deque?)
               (set-rear-ptr! ptr)
               (set-front-ptr! ptr)
               (make-q))
              (else
                (set-previous! ptr rear-ptr)
                (set-next! rear-ptr ptr)
                (set-rear-ptr! ptr)
                (make-q)))))

    (define (rear-delete-queue!)
      (cond ((empty-deque?)
             (error 'rear-delete-queue! "DELETE EMPTY DEQUE"))
            (else
              (let ((new-rear-ptr (previous rear-ptr)))
                (if (not (null? new-rear-ptr))
                  (set-next! new-rear-ptr '()))
                (set-rear-ptr! new-rear-ptr)
                (make-q)))))

    ; Note there is a trivial bugs here. When the deque is empty,
    ; it's possible that front-ptr is null while rear-ptr is not null,
    ; or rear-ptr is null while front-ptr is not null. Therefore,
    ; we have to manage the situation.
    (define (print-deque)
      (define (recur ptr)
        (cond ((not (null? ptr))
               (display (content ptr))
               (display "-")
               (recur (next ptr)))))
      (cond ((empty-deque?)
             (display "empty deque"))
            (else
              (recur front-ptr))))

    (define (dispatch m)
      (cond ((eq? m 'front-insert) front-insert-deque!)
            ((eq? m 'front-delete) front-delete-deque!)
            ((eq? m 'rear-insert) rear-insert-deque!)
            ((eq? m 'rear-delete) rear-delete-queue!)
            ((eq? m 'front) front-deque)
            ((eq? m 'rear) rear-deque)
            ((eq? m 'display) print-deque)
            (else
              (error 'deque-dispatch "UNKNOWN MESSAGE" m))))
    dispatch)
  )

;(front-delete-deque!)
;(rear-delete-deque!)

(define dq (make-deque))
((dq 'front-insert) 3)
((dq 'front-insert) 5)
((dq 'rear-insert) 10)
((dq 'rear-insert) 11)
((dq 'front-delete))
((dq 'rear-insert) 13)
((dq 'rear-delete))
((dq 'rear-delete))
((dq 'display))(newline)
((dq 'front-delete))
((dq 'rear-delete))
((dq 'display))(newline)