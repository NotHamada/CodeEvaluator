#lang racket

(define (is-first-char? str char)
  (and (not (string=? str ""))
       (char=? (string-ref str 0) char)))

; Example usage
(define my-string "hello")

(if (is-first-char? my-string #\h)
    (displayln "The first character is 'h;'")
    (displayln "The first character is not 'h'"))
