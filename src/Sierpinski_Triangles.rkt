#lang racket
; Enter your code here. Read input from STDIN. Print output to STDOUT

#lang racket
; Enter your code here. Read input from STDIN. Print output to STDOUT

(define (triangle m n r c)
    (and
        (>= (modulo r (/ m (expt 2 (sub1 n)))) (/ m (expt 2 n)))
        (or
            (and
                (= n 1)
                (<  (modulo (- c r) (+ m m)) m)
                (>= (modulo (+ r c 1) (+ m m)) m))
            (and
                (> n 1)
                (>= (modulo (- c r) (/ m (expt 2 (- n 2)))) (/ m (expt 2 (sub1 n))))
                (<  (modulo (+ r c 1) (/ m (expt 2 (- n 2)))) (/ m (expt 2 (sub1 n))))))))

(define (test m n r c t)
    (if (zero? n)
        (and
            t
            (>= (+ r c) (sub1 m))
            (<= (- c r) (sub1 m)))
        (test m (sub1 n) r c
            (and
                t
                (not (triangle m n r c))))))

((lambda (m n)
    (for ([r (range m)])
        (for ([c (range (+ m m -1))])
            (if (test m n r c 1)
                (printf "1")
                (printf "_")))
        (printf "~n")))
32 (read))


;((
;    (lambda (f)
;        (lambda (x) (f f x)))
;    (lambda (triangle n)
;        (if (zero? n)
;            (list)
;            (... (triangle triangle (sub1 n))))))
;(read))

