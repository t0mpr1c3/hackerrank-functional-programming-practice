#lang racket
; Enter your code here. Read input from STDIN. Print output to STDOUT
(define (gcd x y)
    (if
        (= x y)
        x
        (if
            (> x y)
            (gcd (- x y) y)
            (gcd (- y x) x))))

(let ([x (read)][y (read)])
    (printf "~a\n" (gcd x y)))

