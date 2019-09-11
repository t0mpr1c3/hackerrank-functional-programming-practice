#lang racket
; Enter your code here. Read input from STDIN. Print output to STDOUT

(define (make-key n m k d)
    (+ d (* 10 (+ 1 k (* 1000 (+ m (* 1000 n)))))))

(define (hash-add! H n m k d p)
    (let ([i (make-key n m k d)])
        ;(if (hash-has-key? H i)
            ;(void)
            (begin
                ;(printf "~a ~a ~a ~a ~a ~a~n" n m k d i p)
                (hash-set! H i p))))
                ;)

(define (hash-get H n m k d)
    (hash-ref H (make-key n m k d)))

; NB zero-indexed for n, m
(let (
    [T (read)]
    [paths (make-hasheqv)])
    (for ([i (range T)])
        (let* (
            [N (read)]
            [M (read)]
            [K (read)]
            [L (max 1 (min (add1 K) (- (+ M N) 2)))])
            (for ([k (range -1 L)])
                (for ([n (range 0 (add1 N))])
                    (for ([m (range 0 (add1 M))])
                        (for ([d (list 1 2)])
                            (if     
                                (or
                                    (< k 0)
                                    (= n 0)
                                    (= m 0))
                                (hash-add! paths n m k d 0)
                                (if (= d 1)
                                    (if (and (= m 1) (= k 0))
                                        (hash-add! paths n 1 0 1 1)
                                        (if (and (= n 1) (= k 0))
                                            (hash-add! paths 1 m 0 1 0)
                                            (if (and (= m 1) (= k 1))
                                                (hash-add! paths n 1 1 1 0)
                                                (if (and (= n 1) (= k 1))
                                                    (hash-add! paths 1 m 1 1 0)
                                                    (hash-add! paths n m k 1
                                                        (+
                                                            (hash-get paths (sub1 n) m k 1)
                                                            (hash-get paths (sub1 n) m (sub1 k) 2)))))))
                                    (if (and (= n 1) (= k 0))
                                        (hash-add! paths 1 m 0 2 1)
                                        (if (and (= m 1) (= k 0))
                                            (hash-add! paths n 1 0 2 0)
                                            (if (and (= n 1) (= k 1))
                                                (hash-add! paths 1 m 1 2 0)
                                                (if (and (= m 1) (= k 1))
                                                    (hash-add! paths n 1 1 2 0)
                                                    (hash-add! paths n m k 2
                                                        (+
                                                            (hash-get paths n (sub1 m) k 2)
                                                            (hash-get paths n (sub1 m) (sub1 k) 1)))))))))))))
            ;(for ([k (range L)])
            ;    (printf "~a ~a ~a: " N M k)
            ;    (printf "~a ~a, "
            ;        (hash-ref paths (make-key N M k 1))
            ;        (hash-ref paths (make-key N M k 2))))
            (if (and (= N 1) (= M 1))
                (printf "1~n")
                (printf "~a~n"
                    (modulo
                        (for/sum ([k (range L)])
                            (+
                                (hash-ref paths (make-key N M k 1))
                                (hash-ref paths (make-key N M k 2))))
                        1000000007))))))