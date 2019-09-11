#lang racket

(require racket/vector)

; Minimum Multiple (LCM)

; calculate primes < 100
(let ([primes (let is-prime ([P (list 2)] [n 3])
                (if (>= n 100)
                    (reverse P)
                    (if (for/and ([p P]) (not (zero? (modulo n p))))
                        (is-prime (cons n P) (add1 n))
                        (is-prime P (add1 n)))))])

  ; factorize numbers < 100
  ; store prime factors in sorted associative arrays
  (let ([factor-arrays (for/vector ([i (range 1 101)])
                         (let factorize ([f '()]
                                         [p primes]
                                         [n i])
                           (cond [(= n 1)
                                  (reverse f)]
                                 [(zero? (modulo n (car p)))
                                  (if (or (empty? f)
                                          (not (= (car p) (caar f))))
                                      (factorize (cons (cons (car p) 1) f)
                                                 p
                                                 (/ n (car p)))
                                      (factorize (cons (cons (car p) (add1 (cdar f))) (cdr f))
                                                 p
                                                 (/ n (car p))))]
                                 [else
                                  (factorize f
                                             (cdr p)
                                             n)])))])
      
    ; count prime factors in associative arrays
    (let ([factor-counts (for/list ([p primes]) (cons p (quotient 99 p)))])
      (let ([total-factor-count (for/sum ([f factor-counts]) (cdr f))])
        
        ; rearrange factors as weighted binary tree
        (define (make-tree entry left-branch right-branch)
          (list entry left-branch right-branch))
        (define (tree-entry tree) (car tree))
        (define (tree-left-branch tree) (cadr tree))
        (define (tree-right-branch tree) (caddr tree))
        (define (take-count elts count [taken '()])
          (if (< count (cdar elts))
              (reverse taken)
              (take-count (cdr elts)
                          (- count (cdar elts))
                          (cons (car elts) taken))))
        (define (partial-assoc-tree elts count)
          (if (zero? count)
              (cons '() elts)
              (let ([left-elts (take-count elts (/ (sub1 count) 2))])
                (let ([left-count (for/sum ([i left-elts]) (cdr i))])
                  (let ([left-result (partial-assoc-tree elts left-count)])
                    (let ([left-tree (car left-result)]
                          [non-left-elts (cdr left-result)])
                      (let ([this-entry (car non-left-elts)])
                        (let ([right-count (- count left-count (cdr this-entry))])
                          (let ([right-result (partial-assoc-tree (cdr non-left-elts) right-count)])
                            (let ([right-tree (car right-result)]
                                  [remaining-elts (cdr right-result)])
                              (cons (make-tree this-entry
                                               left-tree
                                               right-tree)
                                    remaining-elts)))))))))))
        (let ([factor-tree (car (partial-assoc-tree factor-counts total-factor-count))])
          (let ([empty-factor-tree (let zero-tree ([t factor-tree]
                                                   [z '()])
                                     (if (empty? t)
                                         z
                                         (make-tree (cons (car (tree-entry t)) 0)
                                                    (zero-tree (tree-left-branch t) '())
                                                    (zero-tree (tree-right-branch t) '()))))])
              
            ; aggregate two factorized numbers
            ; stored as binary tree
            (define aggregate-factor
              (lambda (func)
                (lambda (factor tree)
                  (cond
                    [(zero? (cdr factor))
                     tree]
                    [(empty? tree)
                     (make-tree factor
                                '()
                                '())]
                    [(= (car factor) (car (tree-entry tree)))
                     (make-tree (cons (car factor) (func (cdr factor) (cdr (tree-entry tree))))
                                (tree-left-branch tree)
                                (tree-right-branch tree))]
                    [(< (car factor) (car (tree-entry tree)))
                     (make-tree (tree-entry tree)
                                ((aggregate-factor func) factor (tree-left-branch tree))
                                (tree-right-branch tree))]
                    [(> (car factor) (car (tree-entry tree)))
                     (make-tree (tree-entry tree)
                                (tree-left-branch tree)
                                ((aggregate-factor func) factor (tree-right-branch tree)))]
                    [else
                     (error "this should not happen")]))))

            ; tree manipulation functions
            (define aggregate-trees
              (lambda (func)
                (lambda (a b)
                  (if (empty? b)
                      a
                      ((agg-trees-rec func) a b)))))
            (define agg-trees-rec
              (lambda (func)
                (lambda (a~ b~)
                  (if (empty? a~)
                      b~
                      ((aggregate-factor func)
                       (tree-entry a~)
                       ((agg-trees-rec func) (tree-right-branch a~)
                                             ((agg-trees-rec func) (tree-left-branch a~)
                                                                   b~)))))))
            (define (prune-tree tree)
              (if (empty? tree)
                  '()
                  (let ([pruned-left  (prune-tree (tree-left-branch  tree))]
                        [pruned-right (prune-tree (tree-right-branch tree))])
                    (cond
                      [(and (zero? (cdr (tree-entry tree)))
                            (empty? pruned-left)
                            (empty? pruned-right))
                       '()]
                      [(and (zero? (cdr (tree-entry tree)))
                            (empty? pruned-left))
                       pruned-right]
                      [(and (zero? (cdr (tree-entry tree)))
                            (empty? pruned-right))
                       pruned-left]
                      [else
                       (make-tree (tree-entry tree) pruned-left pruned-right)]))))              

            ; transform factor exponent data
            ; from associative arrays to binary trees
            (define add-factor (aggregate-factor +))
            (let ([factors
                   (list->vector
                    (let copy-factors ([f (vector->list factor-arrays)][t '()])
                      (if (empty? f)
                          (reverse t)
                          (let ([copied
                                 (let add-factors ([f~ (car f)][t~ empty-factor-tree])
                                   (if (empty? f~)
                                       (prune-tree t~)
                                       (add-factors (cdr f~) (add-factor (car f~) t~))))])
                            (copy-factors (cdr f) (cons copied t))))))])

              ; rebalance tree
              (define (array->tree a)
                (car (partial-tree a (length a))))
              (define (partial-tree elts n)
                (if (= n 0)
                    (cons '() elts)
                    (let ([left-size (quotient (- n 1) 2)])
                      (let ([left-result (partial-tree elts left-size)])
                        (let ([left-tree (car left-result)]
                              [non-left-elts (cdr left-result)]
                              [right-size (- n (+ left-size 1))])
                          (let ([this-entry (car non-left-elts)]
                                [right-result (partial-tree (cdr non-left-elts) right-size)])
                            (let ([right-tree (car right-result)]
                                  [remaining-elts (cdr right-result)])
                              (cons (make-tree this-entry
                                               left-tree
                                               right-tree)
                                    remaining-elts))))))))

              ; tree->array
              (define (tree->array tree)
                (if (empty? tree)
                    '()
                    (append (tree->array (tree-left-branch tree))
                            (list (tree-entry tree))
                            (tree->array (tree-right-branch tree)))))
              
              ; rebalance tree
              (define (rebalance tree)
                (array->tree (tree->array tree)))

              ; defactorize
              (define (defactorize f)
                (foldl (lambda (a b)
                         (modulo
                          (*
                           (modulo
                            (expt (car a) (cdr a))
                            1000000007)
                           b)
                          1000000007))
                       1
                       f))      

              ; find lowest common multiple
              (define lcm (aggregate-trees max))

              ; multiply by addition of exponents
              (define multiply (aggregate-trees +))

              ; scale calculations
              (define (largest-scale idx max-scale [scale 0])
                (if (> scale max-scale)
                    max-scale
                    (if (= 1 (bitwise-and idx 1))
                        scale
                        (largest-scale (quotient idx 2) max-scale (add1 scale)))))
              (define (scale-limit start end [scale-1 0][step 2])
                  (if (> (* (quotient (+ start step) step) step) end)
                      scale-1
                      (scale-limit start end (add1 scale-1) (* 2 step))))                    
              
              ; read data array
              (let ([N (string->number (read-line))])
                (let ([A (list->vector (map string->number (string-split (read-line))))]
                      [log2 (log 2)]
                      [logN (log N)])
                  
                  ; build cache of LCMs at every scale from 2^0 up to N/2 < 2^n <= N
                  ; equivalent to segment tree
                  (let ([max-scale (inexact->exact (floor (/ logN log2)))])
                    (let ([cache
                           (build-vector
                            (add1 max-scale)
                            (lambda (scale)
                              (make-vector (floor (/ N (expt 2 scale))) '())))])

                      ; unit level of cache = data points themselves
                      ; equivalent to leaves of segment tree
                      (vector-copy! (vector-ref cache 0) 0
                                    (for/vector ([a A]) (vector-ref factors (sub1 a))))
                      (for ([scale (range 1 (add1 max-scale))])
                        (vector-copy! (vector-ref cache scale) 0
                                      (for/vector ([j (range (floor (/ N (expt 2 scale))))])
                                        (if (>= scale 1)
                                            (rebalance
                                             (lcm (vector-ref
                                                   (vector-ref cache (sub1 scale))
                                                   (* 2 j))
                                                  (vector-ref
                                                   (vector-ref cache (sub1 scale))
                                                   (add1 (* 2 j)))))
                                            (lcm (vector-ref
                                                  (vector-ref cache (sub1 scale))
                                                  (* 2 j))
                                                 (vector-ref
                                                  (vector-ref cache (sub1 scale))
                                                  (add1 (* 2 j))))))))
                        
                      ; read commands
                      (for ([k (range (string->number (read-line)))])
                        (let ([cmd (string-split (read-line))])
                          (let ([n1 (string->number (cadr  cmd))]
                                [n2 (string->number (caddr cmd))])

                            ; execute command
                            (cond

                              ; span n1 to n2 using the largest scale cache values
                              ; i.e. highest-level intermediate nodes of segment tree
                              [(equal? "Q" (car cmd))
                               (displayln (defactorize
                                            (tree->array
                                             (let span ([j n1][mm '()])
                                               (if (> j n2)
                                                   mm
                                                   (let ([scale (min (largest-scale j max-scale)
                                                                     (scale-limit j n2))])
                                                     (let ([step (expt 2 scale)])
                                                       (span (+ j step)
                                                             (lcm (vector-ref
                                                                   (vector-ref cache scale)
                                                                   (quotient j step))
                                                                  mm)))))))))]

                              ; update cached values at every scale
                              ; i.e. at every level of segment tree
                              [(equal? "U" (car cmd))
                               (let ([new-val (rebalance
                                               (multiply
                                                (vector-ref factors (sub1 n2))
                                                (vector-ref (vector-ref cache 0) n1)))])
                                 (for ([scale (range (add1 max-scale))])
                                   (let ([cache-scale (vector-ref cache scale)]
                                         [step (expt 2 scale)])
                                     (let ([idx (quotient n1 step)])
                                       (when (< idx (quotient N step))
                                         (vector-set!
                                          cache-scale idx
                                          (lcm (vector-ref cache-scale idx)
                                               new-val)))))))]
                                     
                              [else
                               (error "this should not happen")])))))))))))))))
