(import (queue))

(define q (make-queue))
(insert-queue! q 3)
(insert-queue! q 4)
(display (delete-queue! q))(newline)
(display (delete-queue! q))(newline)
(display (delete-queue! q))(newline)
