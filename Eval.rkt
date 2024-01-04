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

(define (print-list lst)
  (for-each
   (lambda (item)
     (displayln (format "Function lines: ~a" item)))
   lst))

(define (calculate lst)
  (define grade 0)
  (define comments (car lst))
  (define lines (car (cdr lst)))
  (define functions (car (cddr lst)))
  (define sum-function-lines (car (cdddr lst)))
  (define variables (car (cddddr lst)))
  (define lst-functions (car (cdr (cddddr lst))))
  (define average (exact->inexact (/ sum-function-lines functions)))

  (displayln (format "Average lines per function: ~a" average))
  (displayln (format "Variables and calls: ~a" variables))
  (displayln (format "Comments: ~a" comments))
  (displayln (format "Total lines: ~a" lines))
  (displayln (format "Total functions: ~a" functions))

  (print-list lst-functions)
)

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
             (lst-function-lines (list))
             (line (read-line input-port)))
    (cond
      [(and (eof-object? line) (< 0 close-parentesis))
        (displayln "Code has wrong syntasis! Final grade is 0!")
        (exit)
      ]
      [(eof-object? line)
       (close-input-port input-port)
       (calculate (list comments line-number number-of-functions sum-function-lines variables lst-function-lines))
       (exit)
       ]
      [(is-empty-line? line)
       (loop line-number function-lines open-parentesis close-parentesis comments number-of-functions sum-function-lines variables lst-function-lines (read-line input-port))]
      [(and (= 1 (verify-comment line))) 
       (loop (add1 line-number) function-lines open-parentesis close-parentesis (add1 comments) number-of-functions sum-function-lines variables lst-function-lines (read-line input-port))]
      [else
       (let* (
              (new-open-parentesis (+ open-parentesis (count-open-parentesis line)))
              (new-close-parentesis (+ close-parentesis (count-close-parentesis line)))
              )
         (when (and (= new-open-parentesis new-close-parentesis) (= function-lines 0))
           (loop (add1 line-number) 0 0 0 comments number-of-functions sum-function-lines (add1 variables) lst-function-lines (read-line input-port)) 
           )
         (when (and (= new-open-parentesis new-close-parentesis) (< 0 new-open-parentesis) (< 0 new-close-parentesis))
           (loop (add1 line-number) 0 0 0 comments (add1 number-of-functions) (+ sum-function-lines (add1 function-lines)) variables (append lst-function-lines (list (add1 function-lines))) (read-line input-port))
           )
         (loop (add1 line-number) (+ function-lines 1) new-open-parentesis new-close-parentesis comments number-of-functions sum-function-lines variables lst-function-lines (read-line input-port)))])))

(define file-path "teste.rkt")

(identify-functions-and-variables file-path)