#lang racket

(define (make-leaf symbol weight)
  (list 'leaf symbol weight))

(define (leaf? obj)
  (and (not (null? obj)) (eq? (car obj) 'leaf)))

(define (symbol-leaf x) (cadr x))

(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define (left-branch tree) (car tree))

(define (right-branch tree) (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
    (list (symbol-leaf tree))
    (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
    (weight-leaf tree)
    (cadddr tree)))

(define (decode bits tree)
  (define (partial-decode bits tree)
    (cond ((leaf? tree) (list (symbol-leaf tree) bits))
          ((null? bits) (error "bits is null but the tree is not leaf."))
          ; left
          ((= (car bits) 0) (partial-decode (cdr bits)
                                            (left-branch tree)))
          ; right
          ((= (car bits) 1) (partial-decode (cdr bits)
                                            (right-branch tree)))
          (else (error "unknown conditions..."))))
  (define (iter bits result)
    (cond ((null? bits) result)
          (else
            (let ((partial-result (partial-decode bits tree)))
              (let ((code (car partial-result))
                    (rest-bits (cadr partial-result)))
                (iter rest-bits (append result (list code))))))))
  (iter bits null))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
    null
    (let ((pair (car pairs)))
      (adjoin-set (make-leaf (car pair)
                             (cadr pair))
                  (make-leaf-set (cdr pairs))))))

(define (encode message tree)
  (if (null? message)
    null
    (append (encode-symbol (car message) tree)
            (encode (cdr message) tree))))

(define (encode-symbol symbol tree)
  (define (symbol-in-tree? tree symbol)
    (if (leaf? tree)
      (eq? (symbol-leaf tree) symbol)
      (or (symbol-in-tree? (left-branch tree) symbol)
          (symbol-in-tree? (right-branch tree) symbol))))
  ; lookup symbol in tree, and return (finded paths), bits store the
  ; temp paths when search the symbol.
  (define (lookup symbol tree bits)
    (if (leaf? tree)
      (if (eq? (symbol-leaf tree) symbol)
        (list #t (reverse bits))
        (list #f (reverse bits)))
      (let ((left-result (lookup symbol
                                 (left-branch tree)
                                 (cons 0 bits))))
        (if (car left-result)
          left-result
          (lookup symbol
                  (right-branch tree)
                  (cons 1 bits))))))
  (let ((lookup-result (lookup symbol tree null)))
    (if (car lookup-result)
      (cadr lookup-result)
      (error "cannot find the symbol"))))

(define (make-huffman-tree freq-pairs)
  (successive-merge (make-leaf-set freq-pairs)))

(define (successive-merge leaf-set)
  (define (merge-recursive leaf-set)
    (cond ((null? (cdr leaf-set)) leaf-set)
          (else (merge-recursive (adjoin-set (make-code-tree (car leaf-set)
                                                             (cadr leaf-set))
                                             (cddr leaf-set))))))
  (car (merge-recursive leaf-set)))

(provide (all-defined-out))
