#lang racket
; Enter your code here. Read input from STDIN. Print output to STDOUT
(define (read-list x)
    (let ([e (read)])
        (if (eof-object? e)
            (reverse x)
            (read-list (cons e x)))))
(define inp (read-list null))
(for-each
	(lambda (x) (printf "~a~%" x))
    (filter 
        (lambda (x) (negative? (- x (car inp))))
        (cdr inp)))