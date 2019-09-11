#lang racket
; Enter your code here. Read input from STDIN. Print output to STDOUT#lang racket
(define (f n) (range n))
(define n
	(string->number
		(read-line
			(current-input-port)
			'any)))
(print (list(f n)))

