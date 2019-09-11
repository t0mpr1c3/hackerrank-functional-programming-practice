#lang racket
; Enter your code here. Read input from STDIN. Print output to STDOUT#lang racket
(define (read-pairs-len n)
    (if (= n 0)
        (list)
            (let* ([x (read)][y (read)])
                (cons (list x y) (read-pairs-len (- n 1))))))
(define (test x)
    (if
        (pair? (cdr x))
        (if
            (and
                (= (caar x) (caadr x))
                (not (= (cadar x) (cadadr x))))
            "NO"
            (test (cdr x)))
        "YES"))
(let ([T (read)])
    (for ([i (range T)])
        (let*
            (
                [N (read)]
                [a (read-pairs-len N)]
                [b (sort (sort a < #:key cadr) < #:key car)])
            (printf "~a\n" (test b)))))