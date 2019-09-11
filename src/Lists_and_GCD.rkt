#lang racket
; Enter your code here. Read input from STDIN. Print output to STDOUT

(define (read-list)
    (let ([x (read-line (current-input-port) 'any)])
        (string-split x)))

(define (hashify x)
    (if (pair? x)
        (cons
            (cons (string->number (car x)) (string->number (cadr x)))
            (hashify (cddr x)))
        (list)))

(define (read-hash n)
    (if (zero? n)
        (list)
        (cons
            (make-hasheqv (hashify (read-list)))
            (read-hash (sub1 n)))))

(define (min-exponent x k)
    (define (exponent i k)
        (if (hash-has-key? i k)
            (hash-ref i k)
            0))
    (inexact->exact
        (foldl min +inf.0
            (map
                (lambda (i) (exponent i k))
                x))))

(let* (
    [q (string->number (car (read-list)))]
    [x (read-hash q)])
    (for ([k (sort (hash-keys (car x)) <)])
        (let ([m (min-exponent x k)])
            (if (zero? m)
                (void)
                (printf "~a ~a " k m)))))
(printf "\n")