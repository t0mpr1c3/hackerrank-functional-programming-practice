#lang 

; iterative lexicographical permuter
(define (next-perm a)
  (let ([n (length a)]
        [a~ (reverse a)])
    (if (<= n 1)
        (void)
        (let-values ([(head tail)
                      (let loop-tail ([x (cdr a~)]
                                      [y (list (car a~))])
                        (if (and (not (empty? x))
                                 (string<? (car y) (car x)))
                            (loop-tail (cdr x)
                                       (cons (car x) y))
                            (values x (reverse y))))])
          (if (empty? head)
              (void)
              (let loop-head ([x '()]
                              [y tail])
                (if (and (not (empty? y))
                         (string<? (car y) (car head)))
                    (loop-head (cons (car y) x)
                               (cdr y))
                    (append (reverse (cdr head))
                            (cons (car y) x)
                            (cons (car head) (cdr y))))))))))

(define (show-permutations a)
  (let permute ([x a])
    (displayln x)
    (let ([x~ (next-perm x)])
      (when (not (void? x~))
        (permute x~)))))

(show-permutations '())
(show-permutations '("a"))
(show-permutations '("a" "b" "c"))
  