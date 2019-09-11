#lang racket
; Enter your code here. Read input from STDIN. Print output to STDOUT

(define (divisor? n p) (= (modulo n p) 0))

(let ([T (read)])
  (for ([t (range T)]) 
    (let ([L (read)]) 
      (let ([M (read)])
        (displayln 
         (for/sum ([i (range 1 (add1 (gcd L M)))]
                   #:when (and (divisor? L i)
                               (divisor? M i)))
           1))))))