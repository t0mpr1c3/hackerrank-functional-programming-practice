#lang racket
  
(define nbst
  (letrec ([memo (make-hasheq '((0 . 1)(1 . 1)(2 . 2)))]
           [f (lambda(x)
                (if (hash-has-key? memo x)
                    (hash-ref memo x)
                    (let ([new-ans (modulo (for/sum ([i (range x)])
                                             (* (nbst i) (nbst (- x i 1))))
                                           100000007)])
                      (hash-set! memo x new-ans)
                      new-ans)))])
           f))


  (let ([T (read)])
    (for ([t (range T)])
      (let ([N (read)])
        (printf "~a~n" (nbst N)))))