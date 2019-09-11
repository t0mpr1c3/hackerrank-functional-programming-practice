#lang racket
; Enter your code here. Read input from STDIN. Print output to STDOUT

(define (bst entry left right) (list entry left right))
(define (bst-entry tree) (car tree))
(define (bst-left tree) (cadr tree))
(define (bst-right tree) (caddr tree))

(define (bst-add tree value)
  (cond
    [(empty? tree)
     (bst value '() '())]
    [(= value (bst-entry tree))
     tree]
    [(< value (bst-entry tree))
     (bst (bst-entry tree) (bst-add (bst-left tree) value) (bst-right tree))]
    [(> value (bst-entry tree))
     (bst (bst-entry tree) (bst-left tree) (bst-add (bst-right tree) value))]))

(define (create tree values)
  (if (empty? values)
      tree
      (begin
        (create (bst-add tree (car values)) (cdr values)))))
                
(define (traverse tree values)
  (if (empty? tree)
      values
      (traverse
       (bst-right tree)
       (traverse
        (bst-left tree)
        (cons (bst-entry tree) values)))))

(let ([T (read)])
  (for ([t (range T)])
    (let ([N (read)])
      (let ([V (for/list ([i (range N)]) (read))])
        (let ([tree (create '() V)])
          (if (equal?
               (reverse (traverse tree '()))
               ;(flatten tree)
               V)
              (printf "YES~n")
              (printf "NO~n")))))))
