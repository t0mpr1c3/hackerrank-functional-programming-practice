#lang racket
; Enter your code here. Read input from STDIN. Print output to STDOUT

(let ([N (read)]
      [intersection (make-hasheq)]
      [difference   (make-hasheq)])
  (for ([i (range N)])
    (let ([a (read)])
      (hash-set! intersection a
                 (if (hash-has-key? intersection a)
                     (add1 (hash-ref intersection a))
                     1))))
                     
  (let ([M (read)])
    (for ([i (range M)])
      (let ([b (read)])
        (if (hash-has-key? intersection b)
            (let ([x (hash-ref intersection b)])
              (if (= x 1)
                  (hash-remove! intersection b)
                  (hash-set! intersection b (sub1 x))))
            (hash-set! difference b
                       (if (hash-has-key? difference b)
                           (add1 (hash-ref difference b))
                           1))))))

  (for ([k (sort (hash-keys difference) <)])
      (printf "~a " k)))