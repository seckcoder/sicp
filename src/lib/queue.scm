(library
  (queue)
  (export make-queue
          empty-queue?
          insert-queue!
          delete-queue!
          front-queue
          beautiful-display-queue)
  (import (chezscheme))

  (define (make-queue) (cons '() '()))
  (define (front-ptr q) (car q))
  (define (set-front-ptr! q item) (set-car! q item))
  (define (rear-ptr q) (cdr q))
  (define (set-rear-ptr! q item) (set-cdr! q item))
  (define (next item) (cdr item))
  (define (content item) (car item))
  (define (front-queue q)
    (if (empty-queue? q)
      (error 'front-queue "FRONT CALLED WITH AN EMPTY QUEUE" q)
      (car (front-ptr q))))
  (define (empty-queue? q) (null? (front-ptr q)))
  (define (insert-queue! q item)
    (let ((new-pair (cons item '())))
      (cond ((empty-queue? q)
             (set-front-ptr! q new-pair)
             (set-rear-ptr! q new-pair)
             q)
            (else
              (set-cdr! (rear-ptr q) new-pair)
              (set-rear-ptr! q new-pair)
              q))))
  (define (delete-queue! q)
    (cond ((empty-queue? q)
           (error 'delete-queue! "DELETE EMPTY QUEUE" q))
          (else
            (set-front-ptr! q (cdr (front-ptr q)))
            q)))
  (define (beautiful-display-queue q)
    (define (recur ptr)
      (if (not (null? ptr))
        (begin
             (display (content ptr))
             (display "-")
             (recur (next ptr)))))
    (recur (front-ptr q)))
  )
