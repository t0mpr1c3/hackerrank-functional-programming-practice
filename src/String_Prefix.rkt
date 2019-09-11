#lang racket
; Enter your code here. Read input from STDIN. Print output to STDOUT


; fast
(let* (
    [A (symbol->string (read))]
    [B (symbol->string (read))]
    [prefix (list->string
        (for/list (
            [a A]
            [b B]
            #:break (not (equal? a b)))
            a))]
    [n (string-length prefix)]
    [A~ (substring A n)]
    [B~ (substring B n)])
    (printf "~a ~a\n" n prefix)
    (printf "~a ~a\n" (string-length A~) A~)
    (printf "~a ~a\n" (string-length B~) B~))


; slow
(let* (
    [A (symbol->string (read))]
    [B (symbol->string (read))])
    (let prefix ([a A][b B][p ""])
        (if
            (or
                (zero? (string-length a))
                (zero? (string-length b))
                (not (equal?
                    (substring a 0 1)
                    (substring b 0 1))))
            (printf "~a ~a~n~a ~a~n~a ~a~n"
                (string-length p) p
                (string-length a) a
                (string-length b) b)
            (prefix
                (substring a 1)
                (substring b 1)
                (string-append p (substring a 0 1))))))

