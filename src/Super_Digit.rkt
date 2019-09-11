#lang racket
; Enter your code here. Read input from STDIN. Print output to STDOUT

(let* ([n (read)][k (read)])
    (let superdigit ([x (* n k)])
        (if (< x 10)
            (printf "~a~n" x)
            (superdigit
                (foldl + 0
                    (map
                        (lambda (i) (string->number (string i)))
                        (string->list (number->string x))))))))