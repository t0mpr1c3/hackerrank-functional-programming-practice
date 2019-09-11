#lang racket
; Enter your code here. Read input from STDIN. Print output to STDOUT

(let ([T (read)])
    (for ([i (range T)])
        (let ([n (read)])
            (printf "~a\n" (* (/ n 2) (sub1 (* 3 n)))))))