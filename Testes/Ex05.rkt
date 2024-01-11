#lang racket

; Quick Sort Algorithm
(define (quick-sort lst)
  (if (null? lst)
      lst
      (let ((pivot (car lst))
            (rest (cdr lst)))
        (append (quick-sort (filter (lambda (x) (< x pivot)) rest))
                (list pivot)
                (quick-sort (filter (lambda (x) (>= x pivot)) rest))))))

; Example usage
(define lst-to-sort '(5 3 8 4 2 7 1 6))

; Quick Sort
(displayln (format "Quick Sort: ~a" (quick-sort lst-to-sort)))
