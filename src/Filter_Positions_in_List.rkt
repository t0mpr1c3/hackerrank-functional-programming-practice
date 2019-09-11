#lang racket
; Enter your code here. Read input from STDIN. Print output to STDOUT
(define (read-list x)
    (let ([e (read)])
        (if (eof-object? e)
            (reverse x)
            (read-list (cons e x)))))
(define inp
	(read-list null))
(define idxs
	(filter 
		odd?
		(range (length inp))))
(define res
	(build-list
		(length idxs)
		(lambda (i) (list-ref inp (list-ref idxs i)))))
(for-each
    (lambda (x) (printf "~a~%" x))
    res)