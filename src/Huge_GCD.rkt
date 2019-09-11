#lang racket
; Enter your code here. Read input from STDIN. Print output to STDOUT

(define (divisor? n p) (= (modulo n p) 0))

; calculate primes < 1000
(let ([primes (let is-prime ([P (list 2)] [n 3])
                (if (>= n 1000)
                    (reverse P)
                    (if (for/and ([p P]) (not (zero? (modulo n p))))
                        (is-prime (cons n P) (add1 n))
                        (is-prime P (add1 n)))))])
  (displayln primes))

  ; factorize numbers < 100
  ; store prime factors in sorted associative arrays
  (define (factorize n [f '()])
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
      
      ; aggregate two factorized numbers
      (define (aggregate func a b [r '()])
        (cond [(empty? a)
               (append (reverse r) b)]
              [(empty? b)
               (append (reverse r) a)]
              [(< (caar a) (caar b))
               (aggregate func
                          (cdr a)
                          b
                          (cons (car a) r))]
              [(> (caar a) (caar b))
               (aggregate func
                          a
                          (cdr b)
                          (cons (car b) r))]
              [(= (caar a) (caar b))
               (aggregate func
                          (cdr a)
                          (cdr b)
                          (cons (cons (caar a) (func (cdar a) (cdar b))) r))]
              [else
               (error "this should not happen")]))

      ; find lowest common multiple
      (define (lcm a b) (aggregate max a b))

      ; multiply
      (define (multiply a b) (aggregate + a b))
      


(let ([N (read)])
  (let ([A (for/list ([i (range N)]) (read))])
(let ([M (read)])
  (let ([B (for/list ([i (range N)]) (read))])