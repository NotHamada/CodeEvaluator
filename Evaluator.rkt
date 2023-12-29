#lang racket

(require syntax/parse)

(define (read-file-line-by-line file-path)
  (with-input-from-file file-path
    (lambda ()
      (let loop ((line (read-line)))
        (cond
          ((eof-object? line) '()) ; End of file reached
          (else
           (displayln line)
           (loop (read-line))))))))

; Example usage
(read-file-line-by-line "teste.rkt")

(define (count-lines-in-file file-path)
  (let ((line-count 0))
    (with-input-from-file file-path
      (lambda ()
        (let loop ((line (read-line)))
          (cond
            ((eof-object? line) line-count) ; Return the line count at the end of file
            (else
             (set! line-count (+ line-count 1))
             (loop (read-line)))))))))

; Example usage
(define file-path "teste.rkt") ; Replace with the path to your file
(displayln (count-lines-in-file file-path))

(define (count-char-occurrences char str)
  (define (count-helper char str count)
    (cond
      [(= (string-length str) 0) count]
      [(char=? (string-ref str 0) char) (count-helper char (substring str 1) (+ count 1))]
      [else (count-helper char (substring str 1) count)]))
  
  (count-helper char str 0))

; Example usage
(define input-string "hello world")
(define char-to-count #\o)

(displayln (format "Occurrences of ~a in ~a: ~a" char-to-count input-string (count-char-occurrences char-to-count input-string)))
