#lang racket
; Enter your code here. Read input from STDIN. Print output to STDOUT
(define fib-hash (make-hasheqv))
(hash-set! fib-hash 0 0)
(hash-set! fib-hash 1 1)
(define (fib n)
    (if
        (hash-has-key? fib-hash n)
        (hash-ref fib-hash n)
        (let ([x (+ (fib (- n 1)) (fib (- n 2)))])
            (hash-set! fib-hash n x)
            x)))

(let ([T (read)])
	(for ([i (range T)])
		(let ([n (read)])
    	(printf "~a\n" (modulo (fib n) 100000007)))))

