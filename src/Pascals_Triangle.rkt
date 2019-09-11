#lang racket
; Enter your code here. Read input from STDIN. Print output to STDOUT

(define fac-hash (make-hasheqv))
(define (fac n)
    (cond
        [(hash-has-key? fac-hash i) (hash-ref fac-hash n)]
        [(zero? n) 1]
        [else
            (let ([x (* n (fac (sub1 n)))])
                (hash-set! fac-hash x)
                x)]))
(define (comb n r)
    (/ (fac n) (* (fac r) (fac (- n r)))))

(let ([N (read)])
    (for ([n (range N)])
        (for ([r (range (add1 n))])
            (printf "~a " (comb n r)))
        (printf "\n")))