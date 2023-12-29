#lang racket

(define (trim-whitespace str)
  (define (is-whitespace? c)
    (char-whitespace? c))
  
  (define (trim-start str)
    (regexp-replace* #rx"^\\s+" str ""))

  (define (trim-end str)
    (regexp-replace* #rx"\\s+$" str ""))

  (trim-end (trim-start str)))