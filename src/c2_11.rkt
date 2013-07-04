

(define (mul-interval x y)
  (let ((x1 (lower-bound x))
        (x2 (upper-bound x))
        (y1 (lower-bound y))
        (y2 (upper-bound y)))
    we have x1 <= x2, y1 <= y2
    => if x1 >=0, then x1y1 <= x1y2
       if x2 >=0, then x2y1 <= x2y2
       if y1 >=0, then x1y1 <= x2y1
       if y2 >=0, then x1y2 <= x2y2
       we can enumerate all the conditions.
    (cond ((and (>= x1 0)
                (>= x2 0)
                (>= y1 0)
                (>= y2 0)) (make-interval (* x1 y1) (* x2 y2)))
          ((and (>= x1 0)
                (>= x2 0)
                (>= y1 0)
                (<= y2 0)) (make-interval (* x1 y1) (* x1 y2)))
          ((and (>= x1 0)
                ...(you can make such kind of thing, but I just don't like
                        such kind of code...)
