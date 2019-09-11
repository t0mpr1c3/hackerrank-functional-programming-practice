#lang racket
; Enter your code here. Read input from STDIN. Print output to STDOUT

(define (findmax-subarray arr len sum l r max-sum max-l max-r)
  (if (= r len)
      (list max-sum max-l max-r)
      (let ([new-sum (+ (vector-ref arr r) sum)])
        (if (< new-sum 0)
            (findmax-subarray arr len 0 (add1 r) (add1 r) max-sum max-l max-r)
            (if (> new-sum max-sum)
                (findmax-subarray arr len new-sum l (add1 r) new-sum l (add1 r))
                (findmax-subarray arr len new-sum l (add1 r) max-sum max-l max-r))))))

(let* (
       [N (read)]
       [K (read)]
       [A (for/list ([i (range N)]) (values (read)))]
       ; first replace sequences of positive and non-positive elements with their sums
       ; returns reversed list, but this does not matter
       [B (let sum-seq ([a A][b '()][acc 0])
            (if (empty? a)
                (if (> acc 0)
                    (cons acc b)
                    ; ignore tail sequence of non-positive elements
                    b)
                (let ([x (car a)])
                  (if (> x 0)
                      (if (>= acc 0)
                          (sum-seq (cdr a) b (+ x acc))
                          (if (empty? b)
                              ; ignore initial sequence of non-positive elements
                              (sum-seq (cdr a) '() x)
                              (sum-seq (cdr a) (cons acc b) x)))
                      (if (<= acc 0)
                          (sum-seq (cdr a) b (+ x acc))
                          (sum-seq (cdr a) (cons acc b) x))))))]
       [C (list->vector (reverse B))])
  ; apply Kadane's algorithm repeatedly
  (let next-subarray ([k K][n (vector-length C)])
    (let ([subarray (findmax-subarray C n 0 0 0 0 0 0)])
      (when (and (> k 0) (> (car subarray) 0))
        (begin
          (printf "~a~n" (car subarray))
          (vector-copy! C (cadr subarray) C (caddr subarray))
          (next-subarray
            (sub1 k)
            (+ n (- (cadr subarray) (caddr subarray)))))))))