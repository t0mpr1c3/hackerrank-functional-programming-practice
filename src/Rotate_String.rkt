#lang racket
; Enter your code here. Read input from STDIN. Print output to STDOUT

(let ([T (read)])
  (for ([t (range T)]) 
    (let ([S (symbol->string (read))])
      (let ([n (string-length S)])
        (let loop ([i n][s S])
          (if (zero? i)
              (printf "~n")
              (let ([s~ (string-append (substring s 1 n)
                                       (substring s 0 1))])
                (printf "~a " s~)
                (loop (sub1 i) s~))))))))