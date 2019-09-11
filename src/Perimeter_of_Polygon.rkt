#lang racket
; Enter your code here. Read input from STDIN. Print output to STDOUT#lang racket
(define (read-pairs-len n)
    (if (= n 0)
        (list)
            (let* ([x (read)][y (read)])
                (cons (list x y) (read-pairs-len (- n 1))))))
(define (perimeter x s)
    (if
        (pair? (cdr x))
        (let*
            (
                [x1 (caar x)]
                [x2 (caadr x)]
                [y1 (cadar x)]
                [y2 (cadadr x)]
                [ed
                    (sqrt
                        (+
                            (expt (- x1 x2) 2)
                            (expt (- y1 y2) 2)))])
            (perimeter (cdr x) (+ s ed)))
        s))
(let*
    (
        [N (read)]
        [x (read-pairs-len N)]
        [y (append x (list(car x)))])
    (printf "~s\n" (perimeter y 0)))