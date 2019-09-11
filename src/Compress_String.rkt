#lang racket
; Enter your code here. Read input from STDIN. Print output to STDOUT


; quick
(let* (
    [msg (symbol->string (read))]
    [pos (regexp-match-positions* #px"(.)\\1{1,}" msg)])
    (let print-msg ([m msg][p pos][n 0])
        (if (null? p)
            (printf "~a~n" m)
            (begin
                (printf "~a~a" (substring m 0 (- (caar p) n -1)) (- (cdar p) (caar p)))
                (print-msg (substring m (- (cdar p) n)) (cdr p) (cdar p))))))


; slow
(let ([P (symbol->string (read))])
    (let compress ([p P][x "#"][n 1])
        (if (zero? (string-length p))
            (if (= n 1)
                (void)
                (printf "~a" n))
            (let ([a (substring p 0 1)])
                (if (equal? x a)
                    (compress (substring p 1) a (add1 n))
                    (begin
                        (if (= n 1)
                            (printf "~a" a)
                            (printf "~a~a" n a))
                        (compress (substring p 1) a 1)))))))
