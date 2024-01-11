#lang racket

; Bubble Sort Algorithm
(define (bubble-sort lst)
  (let loop ((lst lst)
             (changed #f))
    (if (null? lst)
        lst
        (let* ((x (car lst))
               (y (cadr lst)))
          (if (> x y)
              (loop (cons y (cdr lst)) #t)
              (cons x (loop (cdr lst) changed)))))))

; Example usage
(define lst-to-sort '(5 3 8 4 2 7 1 6))

; Bubble Sort
(displayln (format "Bubble Sort: ~a" (bubble-sort lst-to-sort)))
