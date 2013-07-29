(define (make-queue)
  (let ((front-ptr '())
        (rear-ptr '()))
    (define q (cons front-ptr rear-ptr))
    (define (front-queue)
      (if (empty-queue?)
        (error 'front-queue "FRONT CALLED WITH AN EMPTY QUEUE")
        (car front-ptr)))
    (define (empty-queue?) (null? front-ptr))
    (define (set-front-ptr! new-pair)
      (set! front-ptr new-pair))
    (define (set-rear-ptr! new-pair)
      (set! rear-ptr new-pair))
    (define (insert-queue! item)
      (let ((new-pair (cons item '())))
        (cond ((empty-queue?)
               (set-front-ptr! new-pair)
               (set-rear-ptr! new-pair)
               q)
              (else
                (set-cdr! rear-ptr new-pair)
                (set-rear-ptr! new-pair)
                q))))
    (define (delete-queue!)
      (cond ((empty-queue?)
             (error 'delete-queue! "DELETE EMPTY QUEUE"))
            (else
              (set-front-ptr! (cdr front-ptr))
              q)))
    (define (beautiful-display-queue)
      (define (recur ptr)
        (if (not (null? ptr))
          (begin
            (display (car ptr))
            (display "-")
            (recur (cdr ptr)))))
      (recur front-ptr))
    (define (dispatch m)
      (cond ((eq? m 'insert) insert-queue!)
            ((eq? m 'delete) delete-queue!)
            ((eq? m 'display) beautiful-display-queue)
            (else
              (error 'queue-dispatch "UNKNOWN ACTION" m))))
    dispatch))

(define q (make-queue))
((q 'insert) 3)
((q 'insert) 4)
((q 'display))(newline)
((q 'delete))
((q 'display))(newline)
((q 'delete))
((q 'display))
((q 'delete))
