#lang racket
; Enter your code here. Read input from STDIN. Print output to STDOUT

; game of Kayles  #lang racket
(require racket/set)
(require racket/generator)

; The number of runs (heaps) with
; equal length only matters modulo 2
; by applying the symmetry strategy.
; Let us define sets of heaps
; where addition and subtraction are
; performed modulo 2 and are therefore
; equivalent operations
; this is the curried add/remove function
(define (add-heap s)
  (lambda (h)
    (cond ((<= h 0) s)
          ((set-member? s h) (set-remove s h))
          (else (set-add s h)))))
(define remove-heap add-heap)

; returns a generator function that
; iterates over every possible set after the current move
(define partition-maker
  (lambda (s)
    (generator ()
               (for* ([h (in-set s)]
                      [m (in-range 1 3)]
                      [p (in-range (+ 1 (quotient (- h m) 2)))])
                 (let ([n (part s h m p)])
                   ; (printf "~a ~a ~a ~a~n" h m p n)
                   (when n (yield n)))))))

; generate set after removing m pins
; at position p from heap h in set s
(define (part s h m p)
  ((add-heap
    ((add-heap
      ((remove-heap s)
       h))
     p))
   (- h p m)))

; unique key defined by members of set
(define (set-key s)
  (string-join (map number->string
                    (for/list ([i (sort (set->list s) <)]) i)) "-"))

; infer result by induction
; #t = win, #f = loss
; memoize results in hash W
(define (display-result my-set)
  (let ([memo (make-hash)])
    (hash-set! memo "" #f)
    (for ([i (range 1 301)])
      (hash-set! memo (number->string i) #t))
    ;
    (define (win? my-set)
      ; (printf "my turn~n")
      (let ([my-key (set-key my-set)])
        (if (hash-has-key? memo my-key)
            (begin
              ; (printf "stored result: ~a " my-key)
              ; (if (hash-ref memo my-key)
              ;     (printf "winning move~n~n")
              ;     (printf "every move loses~n~n"))
              (hash-ref memo my-key))
            (let ([my-choices (partition-maker my-set)])
              (let ([my-result (winning-move? my-choices #f)])
                ; (printf "storing result: ~a ~a~n" my-set my-result)
                (hash-set! memo my-key my-result)
                my-result)))))
    ;
    (define (winning-move? my-choices i-win)
      (if i-win
          (begin
            ; (printf "found winning move!~n~n")
            #t)
          (let ([next-set (my-choices)])
            (if (void? next-set)
                (begin
                  ; (printf "all moves lose~n~n")
                  #f)
                (winning-move? my-choices (all-reponses-lose? next-set))))))
    ;
    (define (all-reponses-lose? your-set)
      ; (printf "opponent's turn~n")
      (let ([your-key (set-key your-set)])
        (if (hash-has-key? memo your-key)
            (begin
              ; (printf "stored result: ~a " your-key)
              ; (if (hash-ref memo your-key)
              ;     (printf "winning response~n~n")
              ;     (printf "all responses lose~n~n"))
              (not (hash-ref memo your-key)))
            (let ([your-choices (partition-maker your-set)])
              (losing-response? your-choices #f)))))
    ;
    (define (losing-response? your-choices you-lose)
      (if (not you-lose)
          (begin
            ; (printf "found winning response!~n~n")
            #f)
          (let ([next-set (your-choices)])
            (if (void? next-set)
                (begin
                  ; (printf "all responses lose~n~n")
                  #t)
                (losing-response? your-choices (not (win? next-set)))))))
    ;
    (win? my-set)))

; test dataset
(display-result (list->set '(1 2 6)))


(let ([T (read)])
  (for ([t (range T)])
    (let ([N (read)])
      (let ([R (readpins N 0 (make-hasheqv))])
        (if (equal? R '(0 . 0))
            (printf "LOSE~n")
            (printf "WIN~n"))))))