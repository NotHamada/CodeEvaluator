#lang racket

; Selection Sort Algorithm
(define (selection-sort lst)
  (define (find-min lst)
    (if (null? (cdr lst))
        lst
        (let ((min-rest (find-min (cdr lst))))
          (if (< (car min-rest) (car lst))
              min-rest
              lst))))
  
  (let loop ((lst lst)
             (sorted '()))
    (if (null? lst)
        sorted
        (let ((min (find-min lst)))
          (loop (remove min lst) (cons (car min) sorted))))))

; Example usage
(define lst-to-sort '(5 3 8 4 2 7 1 6))

; Selection Sort
(displayln (format "Selection Sort: ~a" (selection-sort lst-to-sort)))
