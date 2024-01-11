#lang racket

; Merge Sort Algorithm
(define (merge-sort lst)
  (define (merge left right)
    (cond
     ((null? left) right)
     ((null? right) left)
     ((<= (car left) (car right))
      (cons (car left) (merge (cdr left) right)))
     (else
      (cons (car right) (merge left (cdr right))))))

  (let ((len (length lst)))
    (if (< len 2)
        lst
        (let* ((mid (/ len 2))
               (left (take lst mid))
               (right (drop lst mid)))
          (merge (merge-sort left) (merge-sort right))))))

; Example usage
(define lst-to-sort '(5 3 8 4 2 7 1 6))

; Merge Sort
(displayln (format "Merge Sort: ~a" (merge-sort lst-to-sort)))
