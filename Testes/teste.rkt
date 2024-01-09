#lang racket

(require srfi/13)

(define final-grade 0)

; Function that  
(define (trim-whitespace str)
  (define (is-whitespace? c) (char-whitespace? c))
  (define (trim-start str) (regexp-replace* #rx"^\\s+" str ""))
  (define (trim-end str) (regexp-replace* #rx"\\s+$" str ""))
  (trim-end (trim-start str)))

(define (is-empty-line? line)
  (string=? "" (trim-whitespace line)))

; Function that count the number of occurrences of a specific char in a string
(define (count-char-occurrences char str)
  (define (count-helper char str count) ; Variable that has the number of occurrences of the char
    (cond
      [(= (string-length str) 0) count] ; If the string is empty, return count that starts as 0
      [(char=? (string-ref str 0) char) (count-helper char (substring str 1) (+ count 1))] ; If the char in position is equal the one we want, count receives + 1
      [else (count-helper char (substring str 1) count)])) ; Else, just continue 

  (count-helper char str 0))

; Function that counts the number of open parentesis
(define (count-open-parentesis line)
  (count-char-occurrences #\( line)
  )

; Function that counts the number of close parentesis
(define (count-close-parentesis line)
  (count-char-occurrences #\) line)
  )

; Function that verify if the line has a comment
(define (verify-comment line)
  (if (count-char-occurrences #\; line) 1 0)
  )

; The grading system is a sum of 2 grades
; The first grade is linked with the total size of the file and comments
; The second grade is linked with the functions sizes and cohesion
; The two grades goes from 1 to 5, 10 being the maximum grade
; If the file has a error, it is a instant 0, since the code won't run

; Function that prints a table of the grading system for the comments
(define (table-comments)
  (displayln "Grading comments table:")
  (displayln "5: 20% - 24%")
  (displayln "4: 15% - 19% or 25% - 29%")
  (displayln "3: 10% - 14% or 30% - 34%")
  (displayln "2:  5% -  9% or 35% - 39%")
  (displayln "1:  0% -  4% or 40+%")
  )

; Function that prints a table of the grading system for the cohesion
(define (table-cohesion)
  (displayln "Grading cohesion table:")
  (displayln "5: 80% - 100%")
  (displayln "4: 60% - 79%")
  (displayln "3: 40% - 59%")
  (displayln "2: 20% - 39%")
  (displayln "1:  0% - 19%")
  )

; Function that grades the code by comments
(define (comments-grade comments lines)
  (define grade 0) ; Variable that initializes the grade
  (define percentage-comments (exact->inexact (* 100 (/ comments lines)))) ; Formula to calculate the percentage of comments

  (newline)

  ; Explanation of how the grading is done 
  (displayln "- Comments grading -")
  (displayln "Formula for the calculation: comments / total lines of file" )
  (displayln (format "= ~a / ~a" comments lines))
  (displayln (format "= ~a" (exact->inexact (/ comments lines))))
  (displayln (format "= ~a %" percentage-comments))

  (newline)

  (table-comments) ; Calls the table

  ; Conditionals for the grading
  (cond
    [(and (>= percentage-comments 20) (< percentage-comments 25))
      (set! grade 5)]
    [(and (>= percentage-comments 15) (< percentage-comments 20) (>= percentage-comments 25) (< percentage-comments 30))
      (set! grade 4)]
    [(and (>= percentage-comments 10) (< percentage-comments 15) (>= percentage-comments 30) (< percentage-comments 35))
      (set! grade 3)]
    [(and (>= percentage-comments 5) (< percentage-comments 10) (>= percentage-comments 35) (< percentage-comments 40))
      (set! grade 2)]
    [else
      (set! grade 1)]
  )

  (displayln (format "Comments grade: ~a of 5" grade)) ; Printing the final grade
  (set! final-grade (+ final-grade grade)) ; Setting in the final grade
)

; Function that grades the code by cohesion
(define (cohesion-grade lst-functions average-lines)
  (define interval (ceiling average-lines)) ; Average of the sizes of functions
  (define max-value (apply max lst-functions)) ; The biggest function
  (define min-value (apply min lst-functions)) ; The smallest function
  (define limits (+ interval (/ (+ min-value max-value) 2))) ; Top limit
  
  (newline)

  ; Explanation of how we did the calculation for the grade
  (displayln "- Cohesion grading -")
  (displayln "Formula for the calculation: average lines by function + ((biggest function + smallest function) / 2)")
  (displayln "and")
  (displayln "((biggest function + smallest function) / 2) - average lines by function")
  (displayln (format "= ~a + ((~a + ~a) / 2)  " interval max-value min-value))
  (displayln (format "= ~a + ~a" interval (/ (+ max-value min-value) 2)))
  (displayln (format "= [~a; ~a]" (- limits interval) limits))

  (newline)

  ; Explaining why the percentage
  (displayln "To reach the percentages, we get the functions there are in the interval.")
  (displayln "Then we do: number of (cohesive functions / total functions) * 100")

  (newline)

  ; Iterate each one of the functions to see if they are cohesive
  (let ((cohesive-functions 0)
        (grade 0))
    (display "Function lines: ")
    (for-each
    (lambda (item)
      (display (format "~a " item)) ; Display the number of lines of the function 
      (cond
        [(and (>= item (- limits interval)) (<= item limits)) ; Conditional to see if it is cohesive
          (set! cohesive-functions (+ cohesive-functions 1))] ; Setting + 1 cohesive functions
        ))
    lst-functions)

    ; Getting the percentage
    (define percentage-cohesion (exact->inexact (* 100 (/ cohesive-functions (length lst-functions)))))

    (newline)

    ; Printing the percentage and the number of cohesive functions
    (displayln (format "Number of cohesive functions: ~a" cohesive-functions))
    (displayln (format "Percentage: ~a / ~a = ~a %" cohesive-functions (length lst-functions) percentage-cohesion))
    
    ; Conditional of cohesion
    (cond
      [(and (>= percentage-cohesion 80))
        (set! grade 5)]
      [(and (>= percentage-cohesion 60) (< percentage-cohesion 80))
        (set! grade 4)]
      [(and (>= percentage-cohesion 40) (< percentage-cohesion 60))
        (set! grade 3)]
      [(and (>= percentage-cohesion 20) (< percentage-cohesion 40))
        (set! grade 2)]
      [else (set! grade 1)]
      )

    ; Calling the table
    (table-cohesion)

    ; Printing the grade and setting the grade
    (displayln (format "Cohesion grade: ~a of 5" grade))
    (set! final-grade (+ final-grade grade))
  )
)

; Function that calculates the grade of the code
; The list contains (in order): 
; Comments, number of lines, number of functions, the sum of all function lines, number of variables (including calls too) and list of the sizes of the functions
(define (calculate lst)
  (define grade 0)
  (define comments (car lst))
  (define lines (car (cdr lst)))
  (define functions (car (cddr lst)))
  (define sum-function-lines (car (cdddr lst)))
  (define variables (car (cddddr lst)))
  (define lst-functions (car (cdr (cddddr lst))))
  (define average-lines (exact->inexact (/ sum-function-lines functions)))

  (displayln (format "Average lines per function: ~a" average-lines)) ; Average lines per function
  (displayln (format "Variables and calls: ~a" variables)) ; How many calls and variables
  (displayln (format "Comments: ~a" comments)) ; Number of comments
  (displayln (format "Total lines: ~a" lines)) ; Total lines of the file
  (displayln (format "Total functions: ~a" functions)) ; And the number of functions

  (comments-grade comments lines) ; Calls the comments grading system
  (cohesion-grade lst-functions average-lines) ; Calls the cohesion grading system

  (newline)

  (displayln (format "Grade: ~a / 10" final-grade)) ; Displays the final grade
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
        (close-input-port input-port)
      ]
      [(and (eof-object? line) (not (eq? line (current-input-port))))
       (close-input-port input-port)
       (calculate (list comments line-number number-of-functions sum-function-lines variables lst-function-lines))
      ]
      [(is-empty-line? line)
       (loop line-number function-lines open-parentesis close-parentesis comments number-of-functions sum-function-lines variables lst-function-lines (read-line input-port))]
      [(not (eq? line (current-input-port)))
       (let (
              (new-open-parentesis (+ open-parentesis (count-open-parentesis line)))
              (new-close-parentesis (+ close-parentesis (count-close-parentesis line)))
              (has-comments (verify-comment line))
              )
              (cond
              [(and (= new-open-parentesis new-close-parentesis) (= function-lines 0))
                (loop (add1 line-number) 0 0 0 (+ comments has-comments) number-of-functions sum-function-lines (add1 variables) lst-function-lines (read-line input-port)) 
              ]
              [(and (= new-open-parentesis new-close-parentesis) (< 0 new-open-parentesis) (< 0 new-close-parentesis) (< 0 function-lines))
                (loop (add1 line-number) 0 0 0 (+ comments has-comments) (add1 number-of-functions) (+ sum-function-lines (add1 function-lines)) variables (append lst-function-lines (list (add1 function-lines))) (read-line input-port))
              ]
              [(not (or (and (= new-open-parentesis new-close-parentesis) (= function-lines 0)) (and (= new-open-parentesis new-close-parentesis) (< 0 new-open-parentesis) (< 0 new-close-parentesis))))
                (loop (add1 line-number) (+ function-lines 1) new-open-parentesis new-close-parentesis (+ comments has-comments) number-of-functions sum-function-lines variables lst-function-lines (read-line input-port))
              ]
              ))])))

; Defining directory that coatains the .rkt files
(define directory-path "/home/mt_hamada/Workspace/CodeEvaluator/Testes")
; Get the files that contains .rkt
(define file-list (directory-list directory-path))

; Get the list and do the process one by one
(for-each
 (lambda (file)
   (set! final-grade 0)
   (displayln file)
   (define file-path (build-path directory-path file))
   (displayln file-path)
   (identify-functions-and-variables file-path)
   (newline))
 file-list)