    #lang racket
    ; Enter your code here. Read input from STDIN. Print output to STDOUT

    (define (automaton balls state)
        (if (empty? balls)
            (zero? state)
            (let (
                [current-ball (first balls)]
                [abs-state (abs state)])
                (if
                    (or
                        (= 2 (modulo abs-state 4))
                        (> abs-state 5))
                    #f
                    (automaton (rest balls)
                        (cond
                            [(equal? current-ball #\R) (+ state 1)]
                            [(equal? current-ball #\G) (- state 1)]
                            [(equal? current-ball #\B) (+ state 4)]
                            [(equal? current-ball #\Y) (- state 4)]
                            [else state]))))))

    (let loop ([T (read)])
        (unless (zero? T)
            (printf "~a~n"
                (if (automaton (string->list (symbol->string (read))) 0)
                    "True"
                    "False"))
            (loop (sub1 T))))
        