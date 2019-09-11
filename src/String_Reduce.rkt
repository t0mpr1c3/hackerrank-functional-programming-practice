#lang racket
; Enter your code here. Read input from STDIN. Print output to STDOUT

; list comprehension
(display
    (for/fold
        (   [acc '()]
            [seen (hash)]
            #:result (list->string acc))
        ([x (in-list (string->list (symbol->string (read))))])
        (cond
            [(hash-ref seen x #f)
                (values acc seen)]
            [else
                (values
                    (cons acc x)
                    (hash-set seen x #t))])))
; kludge
(let* (
    [A (symbol->string (read))]
    [H (for/hash (
        [a (string->list A)]
        [i (range (string-length A))])
        (values a i))]
    [K (hash-keys H)]
    [P (map (lambda (k) (caar (regexp-match-positions (string k) A))) K)]
    [Q (for/list ([k K] [p P]) (cons k p))]
    [R (list->string (map car (sort Q < #:key cdr)))])
    (printf "~a\n" R))