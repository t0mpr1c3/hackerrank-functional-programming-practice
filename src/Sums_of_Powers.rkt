#lang racket
; Enter your code here. Read input from STDIN. Print output to STDOUT

(let ([X (read)])
  (let ([N (read)])
    (let ([terms (for/vector ([i (range X)]
                              #:break (> (expt i N) X))
                   (expt i N))])
      (let ([sum-terms (for/vector ([i (range (vector-length terms))])
                         (for/sum ([j (range (add1 i))])
                           (vector-ref terms j)))])
        (let ([paths (make-vector (add1 X))])
          (for ([i (range (add1 X))])
            (begin
              (vector-set! paths i (make-vector (vector-length terms) -99))
              (let ([paths-i (vector-ref paths i)])
                (for ([j (range (sub1 (vector-length terms)))]
                      #:break (>= (vector-ref sum-terms j) i))
                  (vector-set! paths-i j 0)))))
          (for ([j (range (vector-length terms))]                
                #:break (> (vector-ref sum-terms j) X))
            (vector-set! (vector-ref paths (vector-ref sum-terms j)) j 1))
          (displayln
           (let count-paths ([x X][t (sub1 (vector-length terms))])
             (let ([lookup (vector-ref (vector-ref paths x) t)])
               (if (>= lookup 0)
                   lookup                   
                   (let ([result
                          (if (>= x (vector-ref terms t))
                              (+ (count-paths (- x (vector-ref terms t)) (sub1 t)) 
                                 (count-paths x (sub1 t)))                          
                              (count-paths x (sub1 t)))])
                     (vector-set! (vector-ref paths x) t result)
                     result))))))))))