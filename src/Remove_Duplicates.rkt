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