#lang racket

(define (trim-whitespace str)
  (define (is-whitespace? c)
    (char-whitespace? c))
  
  (define (trim-start str)
    (regexp-replace* #rx"^\\s+" str ""))

  (define (trim-end str)
    (regexp-replace* #rx"\\s+$" str ""))

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

(define (identify-functions-and-variables file-path)
  (define input-port (open-input-file file-path))

  (let loop ((line-number 1)
             (function-lines 0)
             (open-parentesis 0)
             (close-parentesis 0)
             (line (read-line input-port)))
    (cond
      [(eof-object? line)
       (close-input-port input-port)]
      [(or (is-empty-line? line) (= 1 line-number))
       (loop (add1 line-number) function-lines open-parentesis close-parentesis (read-line input-port))]
      [else
       (let* (
              (new-open-parentesis (+ open-parentesis (count-open-parentesis line)))
              (new-close-parentesis (+ close-parentesis (count-close-parentesis line)))
              )
         (when (and (= new-open-parentesis new-close-parentesis) (< 0 new-open-parentesis) (< 0 new-close-parentesis))
           (displayln (format "Line ~a: Function Lines ~a" line-number (if (zero? function-lines) 1 function-lines)))
           (loop (add1 line-number) 0 0 0 (read-line input-port))
           )
         (loop (add1 line-number) (+ function-lines 1) new-open-parentesis new-close-parentesis (read-line input-port)))])))

; Example usage
(define file-path "teste.rkt")

(identify-functions-and-variables file-path)
