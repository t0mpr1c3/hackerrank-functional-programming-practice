#lang racket
; Enter your code here. Read input from STDIN. Print output to STDOUT

(define (read-list)
    (let ([x (read-line (current-input-port) 'any)])
        (string-split x)))

(define (hashify x)
    (define (hashify-helper H x)
        (if (pair? x)
            (let ([i (string->number (car x))])
                (if (hash-has-key? H i)
                    (hash-set! H i (add1 (hash-ref H i)))
                    (hash-set! H i 1))
                (hashify-helper H (cdr x)))
            H))
    (hashify-helper (make-hasheqv) x))

(define (read-hash)
    (hashify (make-hasheqv) (read-list)))

(let ([T (string->number (car (read-list)))])
    (for ([i (range T)])
        (let* (
            [X (read-list)]
            [N (string->number (car X))]
            [K (string->number (cadr X))]
            [A (read-list)]
            [H (hashify A)])
            (for ([k (hash-keys H)])
                (if (< (hash-ref H k) K)
                    (hash-remove! H k)
                    (void)))
            (if (zero? (length (hash-keys H)))
                (printf "-1\n")
                (begin
                    (for ([i A])
                        (let ([j (string->number i)])
                            (if (hash-has-key? H j)
                                (begin
                                    (printf "~s " j)
                                    (hash-remove! H j))
                                (void))))
                    (printf "\n"))))))