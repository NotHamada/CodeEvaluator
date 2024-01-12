#lang racket

(require srfi/13)

(define main-string "This is the main string.")
(define substring-to-find "main")

(if (string-contains? main-string substring-to-find)
    (displayln "Substring found!")
    (displayln "Substring not found."))
