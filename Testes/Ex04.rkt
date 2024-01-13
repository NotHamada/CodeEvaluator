(define zero-acc 0)
(define len-1 1)

;; (mergesort my-list operator) consumes my-list and operator and
;;    produces a sorted my-list with relation to operator.
;; mergesort: (listof Any) (X X -> Bool) -> (listof Any)
;; Examples:
(check-expect (mergesort '(1 2 3) >)
              (list 3 2 1))
(check-expect (mergesort '(1 2 3) <)
              (list 1 2 3))

(define (mergesort my-list operator)
  (local [;; (divider lst count left?) consumes lst, count, and left? and
          ;;     produces split left or right lst.
          ;; divider: (listof Any) Nat Bool -> (listof Any)
          (define (divider lst count left?)
            (cond
              [left?
               (cond
                 [(>= count (length lst)) empty]
                 [else (cons (first lst)
                             (divider (rest lst) (add1 count) true))])]
              [else
               (cond
                 [(>= count (length lst)) lst]
                 [else (divider (rest lst) (add1 count) false)])]))
          ;; (merge oper left right) consumes oper, left, and right and
          ;;    and produces merged list.
          ;; merge: (X X -> Bool) (listof Any) (listof Any) -> (listof Any)
          (define (merge oper left right)
            (cond
              [(zero? (length left)) right]
              [(zero? (length right)) left]
              [(oper (first left) (first right))
               (cons (first left) (merge oper (rest left) right))]
              [else (cons (first right) (merge oper left (rest right)))]))
          ;; (mergesort oper ls) consumes oper and ls and
          ;;    produces a sorted ls with relation to oper.
          ;; mergesort: (X X -> Bool) (listof Any)  -> (listof Any)
          (define (merge-sort oper ls)
            (cond
              [(empty? ls) ls]
              [(= len-1 (length ls)) ls]
              [else (merge oper
                           (merge-sort oper (divider ls zero-acc true))
                           (merge-sort oper (divider ls zero-acc false)))]))]
    (merge-sort operator my-list)))
(check-expect (mergesort '(1 2 3 4 6 7 2) <) (list 1 2 2 3 4 6 7))
(check-expect (mergesort '(1 2 3 10 1237 1271 26 39 0 6 7 2) <)
              (list 0 1 2 2 3 6 7 10 26 39 1237 1271))
(check-expect (mergesort empty <)
              empty)
(check-expect (mergesort '(1 2 3 10 1237 1271 26 39 0 6 7 2) >)
              (list 1271 1237 39 26 10 7 6 3 2 2 1 0))
