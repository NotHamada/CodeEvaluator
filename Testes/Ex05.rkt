#lang racket
(require test-engine/racket-tests)
(require racket/runtime-path)
(require readline/readline)

(define INPUT (file->list ;;"/home/mikesi2/Dropbox/coursera/algorithms/hw1/IntegerArray.txt"))
;; ************ADD ABSOLUTE PATH HERE********          
(check-expect (merge-sort INPUT) (sort INPUT <))

;; ListOfNumbers is one of:
;;  - empty
;;  - (cons Number ListOfNumber)
;; interp. a list of numbers

#;
(define (fn-for-lon lon)
  (cond [(empty? lon) ...]
        [else (... (first lon)
                   (fn-for-lon (rest lon)))]))

;; Template rules used:
;;  - one of: 2 cases
;;  - atomic distinct: empty
;;  - compound: (cons Number ListOfNumber)
;;  - atomic non-distinct: (first lon) is Number
;;  - self-reference: (rest lon) is ListOfNumber

;; ListOfNumber -> ListOfNumber (sorted ascended)
;; merge sorts a list of numbers

(check-expect (merge-sort empty) empty)
(check-expect (merge-sort '(5)) '(5))
(check-expect (merge-sort '(5 3)) '(3 5))
(check-expect (merge-sort '(5 9 7)) '(5 7 9))
(check-expect (merge-sort '(3 6 1 2 8 5)) '(1 2 3 5 6 8))

;(define (merge-sort lon) lon) ;stub

(define (merge-sort lon)
  (cond [(or (= (length lon) 1) (empty? lon))
         lon]
        [else (merge
               (merge-sort (first-half lon))
               (merge-sort (second-half lon)))]))

;; ListOfNumber ListOfNumber -> ListOfNumber
;; merges 2 sorted lists into a larger sorted list
(check-expect (merge empty empty) empty)
(check-expect (merge empty '(5)) '(5))
(check-expect (merge '(5) empty) '(5))
(check-expect (merge '(1 3 6) '(2 5 8)) '(1 2 3 5 6 8))

;(define (merge lon1 lon2) (append lon1 lon2)) ;stub

(define (merge lon1 lon2)
  (if (or (empty? lon1) (empty? lon2))
      (append lon1 lon2)
      (let ([f1 (first lon1)]
            [f2 (first lon2)])
        (if (< f1 f2)
            (cons f1 (merge (rest lon1) lon2))
            (cons f2 (merge lon1 (rest lon2)))))))

;; ListOfNumber -> ListOfNumber
;; returns the 1st half of a LON
;; NOTE: (if odd length, 1st half is shorter than 2nd half)
(check-expect (first-half empty) empty)
(check-expect (first-half '(5)) empty)
(check-expect (first-half '(5 3)) '(5))
(check-expect (first-half '(5 9 7)) '(5))

;(define (first-half lon) lon) ;stub

(define (first-half lon)
  (cond [(or (= (length lon) 1) (empty? lon))
         empty]
        [else (take lon (floor (/ (length lon) 2)))]))

;; ListOfNumber -> ListOfNumber
;; returns the 2nd half of a LON
(check-expect (second-half empty) empty)
(check-expect (second-half '(5)) '(5))
(check-expect (second-half '(5 3)) '(3))
(check-expect (second-half '(5 9 7)) '(9 7))

;(define (second-half lon) lon) ;stub

(define (second-half lon)
  (cond [(empty? lon) empty]
        [else
         (drop lon (floor (/ (length lon) 2)))]))

(test)

(display-lines-to-file (merge-sort INPUT) "/home/mikesi2/Dropbox/coursera/algorithms/hw1/result.txt")