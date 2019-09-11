#lang racket
; Enter your code here. Read input from STDIN. Print output to STDOUT#lang racket
(define (exp x m n p s)
    (if (= n m)
        s
        (exp x m (+ n 1) (* p (/ x n)) (+ s p))))
(define (exponent x)
    (exp x 11 1 1 0))
(define (read-list)
    (let ([x (read)]) 
        (if (eof-object? x)
            (list)
            (cons x (read-list)))))
(let ([inp (read-list)]) 
    (let ([n (car inp)]) 
        (let ([data (cdr inp)])
            (let ([res (map exponent data)])
                (for ([x res])
                    (printf "~a\n" x))))))