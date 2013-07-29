(library
  (queue)
  (export make-queue
          empty-queue?
          insert-queue!
          delete-queue!)
  (import (chezscheme))

  (define (the-empty-queue) '())
  (define (make-queue) (the-empty-queue))
  (define (empty-queue? q) (null? q))
  (define (insert-queue! q item)
    (append! q (list item)))
  (define (delete-queue! q)
    (if (empty-queue? q)
      '()
      (let ((item (car q)))
        (set! q (cdr q))
        item)))
  )
