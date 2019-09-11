#lang racket
; Enter your code here. Read input from STDIN. Print output to STDOUT

(define (make-find-minimal-subset a)
  (let ([data a])
    (let ([subsum-stack (make-vector (add1 (vector-length data)))])
      (let ([top 0])
        ; calculate maximum sums of n integers from dataset
        (define (find-subsum n)
          (if (zero? n)
              0
              (let ([lookup (vector-ref subsum-stack (sub1 n))])
                (if (> lookup 0)
                    lookup
                    (let ([result (+ (vector-ref data (sub1 n))
                                     (find-subsum (sub1 n)))])
                      (vector-set! subsum-stack (sub1 n) result)
                      (when (> n top)
                        (set! top n)))))))
        ; determine size of minimal subset using bisection
        (define (find-minimal-subset s [n0 0][n1 top])
          (if (or (zero? top)
                  (< (vector-ref subsum-stack (sub1 top)) s))
              (if (= top (vector-length data))
                  -1
                  (begin
                    (find-subsum (add1 top))
                    (find-minimal-subset s)))
              (if (= (- n1 n0) 1)
                  n1
                  (let ([n~ (quotient (+ n0 n1) 2)])
                    (if (< (vector-ref subsum-stack (sub1 n~)) s)
                        (find-minimal-subset s n~ n1)
                        (find-minimal-subset s n0 n~))))))
        find-minimal-subset))))

(let ([N (read)])
  (let ([A (for/vector ([i (range N)]) (read))])
    (let ([A~ (vector-sort A >)])
      (let ([find-minimal-subset (make-find-minimal-subset A~)])
        (let ([T (read)])
          (for ([j (range T)])
            (let ([s (read)])
              (displayln (find-minimal-subset s)))))))))