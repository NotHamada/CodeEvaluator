#lang racket

(define testa 1)
(define teste 1)
(define testi 1)
(define testo 1)
(define testu 1)

(define (trim-whitespace str)
  (define (is-whitespace? c)
    (char-whitespace? c))
  
  (define (trim-start str)
    (regexp-replace* #rx"^\\s+" str ""))

  (define (trim-end str)
    (regexp-replace* #rx"\\s+$" str ""))

  (trim-end (trim-start str)))

;
;
;
;
;