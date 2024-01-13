#lang racket

(define (check-type value)
  (cond
    ((list? value) (displayln "It's a list."))
    ((number? value) (displayln "It's a number."))
    (else (displayln "It's neither a list nor a number."))))

; Example usage
(define example-list '(1 2 3))
(define example-number 42)
(define example-string "Hello")

(check-type example-list)   ; Output: It's a list.
(check-type example-number) ; Output: It's a number.
(check-type example-string) ; Output: It's neither a list nor a number.
