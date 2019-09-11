#lang racket
; Enter your code here. Read input from STDIN. Print output to STDOUT#lang racket
(define (read-list x)
    (let ([e (read)])
        (if (eof-object? e)
            (reverse x)
            (read-list (cons e x)))))
(define inp
	(read-list null))
(for/sum ([i (filter odd? inp)]) i)

