#lang racket

(require racket/generator)

(define (splice s)
  (generator ()
             (let outer-loop ([l '()][m (car s)][r (cdr s)])
               (let ([permuter (lperm (append l r))])
                 (let inner-loop ([p (permuter)])
                   (when (not (void? p))
                     (let ([q (cons m p)])
                       (yield q)
                       (inner-loop (permuter))))))
               (if (not (empty? r))
                   (outer-loop (append l (list m)) (car r) (cdr r))
                   (void)))))

(define (lperm s)
  (generator ()
             (cond [(empty? s) (yield '())]
                   [(empty? (cdr s)) (yield s)]
                   [else
                    (let ([splicer (splice s)])
                      (let loop ([q (splicer)])
                        (when (not (void? q))
                          (begin
                            (yield q)
                            (loop (splicer))))))])
             (void)))
                                  
(let ([permuter (lperm '(A B C))])
  (let next-perm ([p (permuter)])
    (when (not (void? p))
      (begin
        (display p)
        (next-perm (permuter))))))
