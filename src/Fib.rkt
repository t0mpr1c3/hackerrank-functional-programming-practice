#lang racket
; Enter your code here. Read input from STDIN. Print output to STDOUT
(define fib-hash (make-hasheqv))
(hash-set! fib-hash 1 0)
(hash-set! fib-hash 2 1)
(define (fib n)
    (if
        (hash-has-key? fib-hash n)
        (hash-ref fib-hash n)
        (let ([x (+ (fib (- n 1)) (fib (- n 2)))])
            (hash-set! fib-hash n x)
            x)))

(let ([n (read)])
    (printf "~a\n" (fib n)))

