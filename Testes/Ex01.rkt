#lang racket

(require srfi/13)

(define final-grade 0)

; Function that trim string

(define (is-empty-line? line)
  (string=? "" (string-trim line)))

(define (identify-tests line)
  (if (string-contains? line "check-") 1 0))

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
  (if (= 1 (count-char-occurrences #\; line)) 1 0)
  )

; The grading system is a sum of 3 grades
; The first grade is linked with the total size of the file and comments, from 1 to 4
; The second grade is linked with the functions sizes and cohesion, from 1 to 3
; The third grade is linked with the number of tests, from 1 to 3
; If the file has a error, it is a instant 0, since the code won't run

; Function that prints a table of the grading system for the comments
(define (table-comments)
  (displayln "Grading comments table:")
  (displayln "4: 25% - 29%")
  (displayln "3: 20% - 24% or 30% - 34%")
  (displayln "2: 15% - 19% or 35% - 39%")
  (displayln "1:      14-% or 44+%")
  )

; Function that prints a table of the grading system for the tests
(define (table-tests)
  (displayln "Grading tests table:")
  (displayln "3: 2 - 5")
  (displayln "2: 6+")
  (displayln "1: 0 - 1")
  )

; Function that prints a table of the grading system for the cohesion
(define (table-cohesion)
  (displayln "Grading cohesion table for more than 1 function:")
  (displayln "3: 67% - 100%")
  (displayln "2: 34% - 66%")
  (displayln "1: 0% - 33%")
  (displayln "Grading cohesion table for 1 function only:")
  (displayln "3: 19-")
  (displayln "2: 20 - 29")
  (displayln "1: 30+")
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
    [(and (>= percentage-comments 25) (< percentage-comments 30))
      (set! grade 4)]
    [(or (and (>= percentage-comments 20) (< percentage-comments 25)) (and (>= percentage-comments 30) (< percentage-comments 35)))
      (set! grade 3)]
    [(or (and (>= percentage-comments 15) (< percentage-comments 20)) (and (>= percentage-comments 35) (< percentage-comments 40)))
      (set! grade 2)]
    [else
      (set! grade 1)]
  )

  (displayln (format "Comments grade: ~a of 4" grade)) ; Printing the final grade
  (set! final-grade (+ final-grade grade)) ; Setting in the final grade
)

; Function that grades the code by cohesion
(define (cohesion-grade lst-functions average-lines)
  (define interval (ceiling average-lines)) ; Average of the sizes of functions
  
  (newline)

  ; Explanation of how we did the calculation for the grade
  (displayln "- Cohesion grading -")
  (displayln "Formula for the calculation: average lines by function + 10")
  (displayln "and")
  (displayln "average lines by function - 10")
  (displayln (format "[~a; ~a]" (- interval 10) (+ interval 10)))
  
  (newline)

  ; Explaining why the percentage
  (displayln "To reach the percentages, we get the functions there are in the interval.")
  (displayln "Then we do: number of (cohesive functions / total functions) * 100")
  (displayln "If there is only one function, the formula is different")

  (newline)

  (cohesion-filter lst-functions interval)
)

; Does the calcultion for cohesion
(define (cohesion-filter lst interval)
  (let ((cohesive-functions 0)
        (grade 0)) 
    (cond 
    [(= 1 (length lst)) ; If there is only one function
      (cond
        [(>= (car lst) 30)
          (set! grade 1)
          (set! final-grade (+ final-grade 1))]
        [(and (< (car lst) 30) (> (car lst) 20))
          (set! grade 2)
          (set! final-grade (+ final-grade 2))]
        [else
          (set! grade 3)
          (set! final-grade (+ final-grade 3))]
      )
      (displayln (format "Function lines: ~a" (car lst))) ; Display the number of lines of the function 

      (table-cohesion)

      (displayln (format "Cohesion grade: ~a of 3" grade))
    ]
    [else 
    (display "Function lines: ")
    (for-each
    (lambda (item)
      (display (format "~a " item)) ; Display the number of lines of the function 
      (cond
        [(and (>= item (- interval 10)) (<= item (+ interval 10))) ; Conditional to see if it is cohesive
          (set! cohesive-functions (+ cohesive-functions 1))] ; Setting + 1 cohesive functions
        ))
    lst)

    ; Getting the percentage
    (define percentage-cohesion (exact->inexact (* 100 (/ cohesive-functions (length lst)))))

    (newline)

    ; Printing the percentage and the number of cohesive functions
    (displayln (format "Number of cohesive functions: ~a" cohesive-functions))
    (displayln (format "Percentage: ~a / ~a = ~a %" cohesive-functions (length lst) percentage-cohesion))
    
    ; Conditional of cohesion
    (cond
      [(and (>= percentage-cohesion 67))
        (set! grade 3)]
      [(and (>= percentage-cohesion 34) (< percentage-cohesion 67))
        (set! grade 2)]
      [else (set! grade 1)]
      )

    ; Calling the table
    (table-cohesion)

    ; Printing the grade and setting the grade
    (displayln (format "Cohesion grade: ~a of 3" grade))
    (set! final-grade (+ final-grade grade))]
  )))

; Function that grades the code by tests
(define (tests-grade number-of-tests number-of-functions)
  (let ((grade 0)
        (average (exact->inexact (/ number-of-tests number-of-functions))))
    
    (newline)
    
    (displayln "- Tests grading -")
    (displayln "Formula for the calculation: tests / number of functions" )
    (displayln (format "= ~a / ~a" number-of-tests number-of-functions))
    (displayln (format "= ~a" (exact->inexact (/ number-of-tests number-of-functions))))

    (cond 
      [(and (>= average 2) (<= average 5))
        (set! grade 3)]
      [(>= average 6)
        (set! grade 2)]
      [else (set! grade 1)]
    )

    (newline)
    (table-tests) ; Calls the table

    (displayln (format "Comments grade: ~a of 3" grade)) ; Printing the final grade
    (set! final-grade (+ final-grade grade)) ; Setting in the final grade
  ))

; Function that calculates the grade of the code
(define (calculate lst)
  (define grade 0)
  (define comments (car lst))
  (define lines (car (cdr lst)))
  (define functions (car (cddr lst)))
  (define sum-function-lines (car (cdddr lst)))
  (define variables (car (cddddr lst)))
  (define lst-functions (car (cdr (cddddr lst))))
  (define tests (car (cddr (cddddr lst))))
  (define average-lines (exact->inexact (/ sum-function-lines functions)))

  (displayln (format "Average lines per function: ~a" average-lines)) ; Average lines per function
  (displayln (format "Variables and calls: ~a" variables)) ; How many calls and variables
  (displayln (format "Comments: ~a" comments)) ; Number of comments
  (displayln (format "Total lines: ~a" lines)) ; Total lines of the file
  (displayln (format "Total functions: ~a" functions)) ; Number of functions
  (displayln (format "Total tests ~a" tests)) ; And the number of tests

  (comments-grade comments lines) ; Calls the comments grading system
  (cohesion-grade lst-functions average-lines) ; Calls the cohesion grading system
  (tests-grade tests functions) ; Calls the tests grading system

  (newline)

  (displayln (format "Grade: ~a / 10" final-grade)) ; Displays the final grade
)

; Function that identifies all the things that are in the file
(define (identify-functions-and-variables file-path)
  (define input-port (open-input-file file-path)) ; Get file

  ; Starts a iteration until the end of file, with some important variables
  (let loop ((line-number 1)
             (function-lines 0)
             (open-parentesis 0)
             (close-parentesis 0)
             (comments 0)
             (number-of-functions 0)
             (number-of-tests 0)
             (sum-function-lines 0)
             (variables 0)
             (lst-function-lines (list))
             (line (read-line input-port)))
    ; Here we have a cond 
    (cond
      [(and (eof-object? line) (< 0 close-parentesis)) ; Code with wrong syntasis is a 0 
        (displayln "Code has wrong syntasis! Final grade is 0!")
        (close-input-port input-port)
      ]
      [(and (eof-object? line) (not (eq? line (current-input-port)))) ; File ended
       (close-input-port input-port)
       (calculate (list comments line-number number-of-functions sum-function-lines variables lst-function-lines number-of-tests))
      ]
      [(is-empty-line? line) ; Line is empty
       (loop line-number function-lines open-parentesis close-parentesis comments number-of-functions number-of-tests sum-function-lines variables lst-function-lines (read-line input-port))]
      [(not (eq? line (current-input-port))) ; Line not empty, so we have another cond inside to verify some conditions 
       (let (
              (new-open-parentesis (+ open-parentesis (count-open-parentesis line)))
              (new-close-parentesis (+ close-parentesis (count-close-parentesis line)))
              (has-comments (verify-comment line))
              (has-tests (identify-tests line))
              ) ; Variables that verify if there is parentesis, or comments
              (cond
              [(and (= new-open-parentesis new-close-parentesis) (= function-lines 0)) ; Verify if it is a call or variable
                (identify-tests line)
                (loop (add1 line-number) 0 0 0 (+ comments has-comments) number-of-functions (+ number-of-tests has-tests) sum-function-lines (add1 variables) lst-function-lines (read-line input-port)) 
              ]
              [(and (= new-open-parentesis new-close-parentesis) (< 0 new-open-parentesis) (< 0 new-close-parentesis) (< 0 function-lines)) ; Verify if a function has ended
                (identify-tests line)
                (loop (add1 line-number) 0 0 0 (+ comments has-comments) (add1 number-of-functions) (+ number-of-tests has-tests) (+ sum-function-lines (add1 function-lines)) variables (append lst-function-lines (list (add1 function-lines))) (read-line input-port))
              ]
              [(not (or (and (= new-open-parentesis new-close-parentesis) (= function-lines 0)) (and (= new-open-parentesis new-close-parentesis) (< 0 new-open-parentesis) (< 0 new-close-parentesis)))) ; Verify if it still in the function or it is a function
                (identify-tests line)
                (loop (add1 line-number) (+ function-lines 1) new-open-parentesis new-close-parentesis (+ comments has-comments) number-of-functions (+ number-of-tests has-tests) sum-function-lines variables lst-function-lines (read-line input-port))
              ]
              ))])))

; Defining directory that coatains the .rkt files
(define directory-path "Testes")

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

; Testes Unitários
(module+ test
  (require rackunit)

  ; Testes para is-empty-line?
  (check-true (is-empty-line? ""))
  (check-true (is-empty-line? "   "))
  (check-false (is-empty-line? "not empty"))

  ; Testes para count-char-occurrences
  (check-equal? (count-char-occurrences #\a "banana") 3)
  (check-equal? (count-char-occurrences #\z "banana") 0)
  (check-equal? (count-char-occurrences #\a "") 0)  ; Caso de string vazia

  ; Testes para count-open-parentesis
  (check-equal? (count-open-parentesis "(abc (def)") 2)
  (check-equal? (count-open-parentesis "no parentheses") 0)
  (check-equal? (count-open-parentesis "") 0)  ; Caso de string vazia

  ; Testes para count-close-parentesis
  (check-equal? (count-close-parentesis "(abc) def)") 1)
  (check-equal? (count-close-parentesis "no parentheses") 0)
  (check-equal? (count-close-parentesis "") 0)  ; Caso de string vazia

  ; Testes para verify-comment
  (check-equal? (verify-comment "; this is a comment") 1)
  (check-equal? (verify-comment "this is not a comment") 0)
  (check-equal? (verify-comment "") 0)  ; Caso de string vazia
)