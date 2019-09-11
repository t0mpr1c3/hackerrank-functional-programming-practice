#lang racket
; Enter your code here. Read input from STDIN. Print output to STDOUT


; area under polynomial a_1.x^b_1 + ... + a_N.x^b_N between L and R
(define (area a b L R s)
    (let* ([dx 0.001][L_ (+ dx L)])
        (if
            (< (abs (- L_ R)) 0.0001)
            s
            (area a b L_ R (+ s (func a b L_ dx 1 0))))))

; numerical integration function
(define (func a b x dx i s)
    (if
        (pair? a)
        (* dx s)
        (func (cdr a) (cdr b) x dx (+ i 1) (+ s (* (car a) (expt x (car b)))))))

; function to read data points from space delimited string
(define (read-list)
    (let ([x (read-line (current-input-port) 'any)])
        (string-split x)))

; main
; read data, calculate and print output
(let*
    (
        [a (read-list)]
        [b (read-list)]
        [L (read)]
        [R (read)])
    (printf "~s\n" (area a b L R 0)))
