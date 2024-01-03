#lang racket

(define (trim-whitespace str)
  (define (is-whitespace? c) (char-whitespace? c))
  (define (trim-start str) (regexp-replace* #rx"^\\s+" str ""))
  (define (trim-end str) (regexp-replace* #rx"\\s+$" str ""))
  (trim-end (trim-start str)))

(define (is-empty-line? line)
  (string=? "" (trim-whitespace line)))

(define (count-char-occurrences char str)
  (define (count-helper char str count)
    (cond
      [(= (string-length str) 0) count]
      [(char=? (string-ref str 0) char) (count-helper char (substring str 1) (+ count 1))]
      [else (count-helper char (substring str 1) count)]))

  (count-helper char str 0))

(define (count-open-parentesis line)
  (count-char-occurrences #\( line)
  )

(define (count-close-parentesis line)
  (count-char-occurrences #\) line)
  )

(define (verify-comment line)
  (count-char-occurrences #\; line)
  )

(define (is-first-char? str)
  (and (not (string=? str ""))
       (char=? (string-ref str 0) #\; )))

(define (calculate lst)
  (define grade 0)
  (define comments (car lst))
  (define lines (car (cdr lst)))
  (define functions (car (cddr lst)))
  (define sum-function-lines (car (cdddr lst)))
  (define variables (car (cddddr lst)))
  (define average (exact->inexact (/ sum-function-lines functions)))

  (displayln (format "Average lines per function: ~a" average))
  (displayln (format "Variables: ~a" variables))

  (cond
    [(< average 5) (displayln "Código ruim")]
    [(and (> average 5) (< average 10)) (displayln "Tropa do código médio")]
    )

)

; Exemplo de acesso aos itens da lista: (car lst) acessa o primeiro item
; Exemplo de acesso aos itens da lista: (car (cdr lst)) acessa o segundo item
; Exemplo de acesso aos itens da lista: (car (cddr lst)) acessa o terceiro item


(define (identify-functions-and-variables file-path)
  (define input-port (open-input-file file-path))

  (let loop ((line-number 1)
             (function-lines 0)
             (open-parentesis 0)
             (close-parentesis 0)
             (comments 0)
             (number-of-functions 0)
             (sum-function-lines 0)
             (variables 0)
             (line (read-line input-port)))
    (cond
      [(eof-object? line)
       (close-input-port input-port)
       (calculate (list comments line-number number-of-functions sum-function-lines variables))
       (exit)
       ]
      [(or (is-empty-line? line) (= 1 line-number))
       (loop (add1 line-number) function-lines open-parentesis close-parentesis comments number-of-functions sum-function-lines variables (read-line input-port))]
      [(and (= 1 (verify-comment line)) (is-first-char? line)) 
       (loop (add1 line-number) function-lines open-parentesis close-parentesis (add1 comments) number-of-functions sum-function-lines variables (read-line input-port))]
      [else
       (let* (
              (new-open-parentesis (+ open-parentesis (count-open-parentesis line)))
              (new-close-parentesis (+ close-parentesis (count-close-parentesis line)))
              )
         (when (and (= new-open-parentesis new-close-parentesis) (= new-close-parentesis 1) (= function-lines 0))
           (loop (add1 line-number) 0 0 0 comments number-of-functions (+ sum-function-lines function-lines) (add1 variables) (read-line input-port)) 
           )
         (when (and (= new-open-parentesis new-close-parentesis) (< 0 new-open-parentesis) (< 0 new-close-parentesis))
           ; Achar um jeito de identificar variavel: quando open-parentesis é 1 e close-parentesis é 1 
           (displayln (format "Line ~a: Function Lines ~a" line-number (if (zero? function-lines) 1 function-lines)))
           (loop (add1 line-number) 0 0 0 comments (add1 number-of-functions) (+ sum-function-lines function-lines) variables (read-line input-port))
           )
         (loop (add1 line-number) (+ function-lines 1) new-open-parentesis new-close-parentesis comments number-of-functions sum-function-lines variables (read-line input-port)))])))

; Example usage
(define file-path "teste.rkt")

(identify-functions-and-variables file-path)