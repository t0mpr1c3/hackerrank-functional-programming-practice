#lang racket
; Enter your code here. Read input from STDIN. Print output to STDOUT

; KMP algorithm
; https://en.wikipedia.org/wiki/Knuth%E2%80%93Morris%E2%80%93Pratt_algorithm

(require racket/fixnum)
;
(define (read-str)
    (read-line (current-input-port) 'any))
;
(let loop ([count (string->number (read-str))])
    (unless (zero? count)
        (let* (
            [S (list->vector (string->list (read-str)))]
            [W (list->vector (string->list (read-str)))]
            [M (vector-length W)]
            [N (vector-length S)]
            [T (make-fxvector (add1 M))])
            ;
            (define (locate-shifts)
                ;
                (define (locate-shift pos cnd)
                    ;(printf "~a ~a ~a~n" pos cnd t)
                    (if (< pos M)
                        (if (eq? (vector-ref W pos) (vector-ref W cnd))
                            (begin
                                (fxvector-set! T pos (fxvector-ref T cnd))
                                (locate-shift
                                    (add1 pos)
                                    (add1 cnd)))
                            (begin
                                (fxvector-set! T pos cnd)
                                (locate-shift
                                    (add1 pos)
                                    (backtrack pos (fxvector-ref T cnd)))))
                        (fxvector-set! T pos cnd)))
                ;
                (define (backtrack pos cnd)
                    (if
                        (or
                            (< cnd 0)
                            (eq? (vector-ref W pos) (vector-ref W cnd)))
                        (add1 cnd)
                        (backtrack pos (fxvector-ref T cnd))))
                ;
                (fxvector-set! T 0 -1)
                (locate-shift 1 0))
            ;
            (locate-shifts)
            ;(printf "~a~n" T)
            (let match ([j 0][k 0])
                (if (= k M)
                    (display "YES\n")
                    (if (< j N)
                        (if (eq?
                                (vector-ref W k)
                                (vector-ref S j))
                            (match (add1 j) (add1 k))
                            (let ([tk (fxvector-ref T k)])
                                (if (< tk 0)
                                    (match (add1 j) 0)
                                    (match j tk))))
                        (display "NO\n")))))
        ;
        (loop (sub1 count))))