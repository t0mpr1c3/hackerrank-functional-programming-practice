#lang racket
; Enter your code here. Read input from STDIN. Print output to STDOUT

(define (lcm a b) (/ (* a b) (gcd a b)))

(let ([N (read)])
  (let ([J (for/list ([i (range N)]) (read))])
    (display (foldl lcm 1 J))))