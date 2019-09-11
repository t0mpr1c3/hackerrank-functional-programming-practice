#lang racket
; Enter your code here. Read input from STDIN. Print output to STDOUT

; combinations

(define (make-lemurs)
  (let ([memo (make-vector 1001)])
    (for ([i (range 1001)])
      (vector-set! memo i (make-vector (add1 i) -1)))
    (define (lemurs n k)
      (let ([memo-n (vector-ref memo n)])
        (let ([lookup (vector-ref memo-n k)])
          (if (>= lookup 0)
              lookup
              (let ([result
                     (cond [(= n 0) 0]
                           [(= k 0) 1]
                           [(= n k) 1]
                           [else                       
                            (modulo (+ (lemurs (sub1 n) k)
                                       (lemurs (sub1 n) (sub1 k)))
                                    100000007)])])
                (vector-set! memo-n k result)
                result)))))
    lemurs))

(let ([lemurs (make-lemurs)])
  (for ([t (range (read))])
    (let ([n (read)])
      (let ([k (read)])
        (displayln (lemurs n k))))))
