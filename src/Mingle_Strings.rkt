#lang racket
; Enter your code here. Read input from STDIN. Print output to STDOUT


; these are all equivalent:

; shortest and quickest
(for (
    [p (symbol->string (read))]
    [q (symbol->string (read))])
    (printf "~a~a" p q))


(let* (
    [P (symbol->string (read))]
    [Q (symbol->string (read))])
    (((lambda (f)
        (lambda (p q) (f f p q)))
    (lambda (mingle p q)
        (if (zero? (string-length p))
            (printf "~n")
            (begin
                (printf "~a~a"
                    (substring p 0 1)
                    (substring q 0 1))
                (mingle mingle
                    (substring p 1)
                    (substring q 1))))))
    P Q))


(let* (
    [P (symbol->string (read))]
    [Q (symbol->string (read))])
    (letrec ([mingle
        (lambda (p q)
            (if (zero? (string-length p))
                (printf "~n")
                (begin
                    (printf "~a~a"
                        (substring p 0 1)
                        (substring q 0 1))
                    (mingle
                        (substring p 1)
                        (substring q 1)))))])
        (mingle P Q)))


(define (mingle p q)
    (if (zero? (string-length p))
        (printf "~n")
        (begin
            (printf "~a~a"
                (substring p 0 1)
                (substring q 0 1))
            (mingle
                (substring p 1)
                (substring q 1)))))
(let* (
    [P (symbol->string (read))]
    [Q (symbol->string (read))])
    (mingle P Q))


(let* (
    [P (symbol->string (read))]
    [Q (symbol->string (read))])
    (let mingle ([p P][q Q])
    (if (zero? (string-length p))
        (printf "~n")
        (begin
            (printf "~a~a"
                (substring p 0 1)
                (substring q 0 1))
            (mingle
                (substring p 1)
                (substring q 1))))))

