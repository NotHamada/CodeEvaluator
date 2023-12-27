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

(define (identify-functions stx)
  (define line-counter 0) ; Initialize line counter

  (define (count-lines stx)
    (syntax-parse stx
      [(? identifier?)
       (set! line-counter (add1 line-counter))
       #'stx]
      [else
       (syntax-case stx ()
         [(_ . _)
          (set! line-counter (add1 line-counter))
          (syntax/loc stx #'(begin . _))]
         [_ stx])]))

  (syntax-parse stx
    [(_ (define (name args ...) body ...))
     (let ((body-stx (count-lines #'(body ...))))
       (displayln (format "Function found: ~a" #'name))
       (displayln (format "Number of lines in the function: ~a" line-counter))
       #'(define (name args ...) body-stx))]
    [(_ (define name expr))
     (displayln (format "Variable found: ~a" #'name))
     #'(define name expr)]
    [(_ _ ...)
     #f])) ; Ignore other expressions

(define (parse-file file-path)
  (define input-port (open-input-file file-path))
  (let loop ()
    (define stx (read-syntax #f input-port))
    (cond
      [(eof-object? stx) (close-input-port input-port)] ; End of file reached
      [else
       (identify-functions stx)
       (loop)])))
; Example usage
(parse-file "teste.rkt")