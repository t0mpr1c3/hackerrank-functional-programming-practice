#lang racket

(require racket/generator)

; permutes list x using Heap's algorithm
(define (make-permuter a)
  (generator ()
             (let ([n (length a)])
               (yield a)
               (let permute ([x a]
                             [k 0]
                             [c (make-list n 0)])
                 (when (< k n)
                   (let-values ([(c-head c-tail) (split-at c k)])
                     (let ([ck (car c-tail)])
                       (if (< ck k)                         
                           (let ([x~ (swap x (if (odd? k) ck 0) k)])
                             (yield x~)
                             (permute x~
                                      0
                                      (append c-head (cons (add1 ck) (cdr c-tail)))))
                           (permute x
                                    (add1 k)
                                    (append c-head (cons 0 (cdr c-tail))))))))))))                 

(let ([P (make-permuter '(1 2 3 4))])
  (let repeat ()
    (let ([p (P)])
      (displayln p)
      (when (not (void? p))
        (repeat)))))