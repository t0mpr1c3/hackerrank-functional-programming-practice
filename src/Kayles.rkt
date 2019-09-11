#lang racket
; Enter your code here. Read input from STDIN. Print output to STDOUT

; game of Kayles

; Kayles moves
; https://en.wikipedia.org/wiki/Kayles

(define (moves n)
  (for*/list ([m (in-range 1 3)]
              [p (in-range (+ 1 (quotient (- n m) 2)))])
    (cons p (max 0 (- n m p)))))


; mex function
; https://en.wikipedia.org/wiki/Mex_(mathematics)

(define (mex n s)
  (for/first ([ i (range (add1 n))] #:when (not (index-of s i))) i))


; Sprague-Grundy theorem
; https://en.wikipedia.org/wiki/Sprague%E2%80%93Grundy_theorem

; Nimbers calculated recursively 
; https://en.wikipedia.org/wiki/Nimber

(define (grundy n [calculate #f])
  (let ([memo (make-hasheq '((0 . 0)))])
    (define (calculate-grundy i)
      (mex i (map (lambda (x) (bitwise-xor (hash-ref memo (car x))
                                           (hash-ref memo (cdr x))))
                  (moves i))))
    (when calculate
      (for ([i (remove* (hash-keys memo) (range (add1 n)))])
        (hash-set! memo i (calculate-grundy i))))
    (cond [(hash-ref memo n #f) => values]
          [else (grundy n #t)])))


; definition of winning position

(define (nim-add x) (foldl bitwise-xor 0 (map grundy x)))

(define (win? x)
  (not (zero? (nim-add x))))


; verify the first few terms in the series
; https://oeis.org/A002186
; calculation is very slow after the first 16 or so

; (for ([n (range 1 13)])
;     (printf "Grundy ~a = ~a~n" n (grundy n)))


; read data and display results

(for ([t (range (string->number (read-line)))])
  (read-line)
  (displayln 
    (if (win? (remq 0 (map string-length (regexp-split "X" (read-line)))))
      "WIN" "LOSE")))