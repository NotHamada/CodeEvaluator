#lang racket

; Insertion Sort Algorithm
(define (insertion-sort lst)
  (define (insert item sorted)
    (cond
     ((null? sorted) (list item))
     ((<= item (car sorted)) (cons item sorted))
     (else (cons (car sorted) (insert item (cdr sorted))))))
  
  (let loop ((lst lst)
             (sorted '()))
    (if (null? lst)
        sorted
        (loop (cdr lst) (insert (car lst) sorted)))))

; Example usage
(define lst-to-sort '(5 3 8 4 2 7 1 6))

; Insertion Sort
(displayln (format "Insertion Sort: ~a" (insertion-sort lst-to-sort)))
