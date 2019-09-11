#lang racket
; Enter your code here. Read input from STDIN. Print output to STDOUT

(for ([t (range (read))])
    (let ([p (symbol->string (read))])
        (for (
            [a p]
            [b (substring p 1)]
            [i (range (string-length p))]
            #:when (even? i))
            (printf "~a~a" b a))
        (printf "~n")))