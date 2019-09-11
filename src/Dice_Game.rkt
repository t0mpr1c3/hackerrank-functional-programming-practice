#lang racket
; Enter your code here. Read input from STDIN. Print output to STDOUT

; 24 orientations of die determined by top & right faces
; transitions
; from    12 13 14 15  21 23 24 26  31 32 35 36  41 42 45 46  51 53 54 56  62 63 64 65
; down    42 23 54 35  31 63 14 46  51 12 65 26  21 62 15 56  41 13 64 36  32 53 24 45
; right   51 41 31 21  62 42 32 12  63 53 23 13  64 54 24 14  65 45 35 15  56 46 36 26
; or
; from     0  1  2  3   4  5  6  7   8  9 10 11  12 13 14 15  16 17 18 19  20 21 22 23
; down    13  5 18 10   8 21  2 15  16  0 23  7   4 20  3 19  12  1 22 11   9 17  6 14
; right   16 12  8  4  20 13  9  0  21 17  5  1  22 18  6  2  23 14 10  3  19 15 11  7

(let ([face   '#(12 13 14 15  21 23 24 26  31 32 35 36  41 42 45 46  51 53 54 56  62 63 64 65)]
      [points '#( 1  1  1  1   2  2  2  2   3  3  3  3   4  4  4  4   5  5  5  5   6  6  6  6)]
      [down   '#(13  5 18 10   8 21  2 15  16  0 23  7   4 20  3 19  12  1 22 11   9 17  6 14)]
      [right  '#(16 12  8  4  20 13  9  0  21 17  5  1  22 18  6  2  23 14 10  3  19 15 11  7)])

  (define (move state dir); down=0 right=1
    (let ([new-state (if (= dir 0)
                         (vector-ref down state)
                         (vector-ref right state))])
      (let ([score (vector-ref points new-state)])
        (cons score new-state))))

  (define (score-moves state dirs [score 0]); dirs in list
    (if (empty? dirs)
        (cons score state)
        (let ([result (move state (car dirs))])
          (score-moves (cdr result) (cdr dirs) (+ score (car result))))))

  (define (make-score m n)
    (let ([cache (make-vector m)])
      (for ([i (range m)])
        (vector-set! cache i (make-vector n '(-99 . 0))))
      (vector-set! (vector-ref cache 0) 0 '(1 . 2))
      (define score
        (lambda (m n)
          (let ([lookup (vector-ref (vector-ref cache m) n)])
            (if (> (car lookup) 0)
                lookup
                (let ([prev-down        (if (> m 0) (score (sub1 m) n) '(-99 . 0))]
                      [prev-right       (if (> n 0) (score m (sub1 n)) '(-99 . 0))])
                  (let ([state-down     (vector-ref down  (cdr prev-down ))]
                        [state-right    (vector-ref right (cdr prev-right))])
                    (let ([points-down  (vector-ref points state-down )]
                          [points-right (vector-ref points state-right)])
                      (let ([result
                             (cond [(= n 0)
                                    (cons (+ (car prev-down) points-down) state-down)]
                                   [(= m 0)
                                    (cons (+ (car prev-right) points-right) state-right)]
                                   [(> points-down points-right)
                                    (cons (+ (car prev-down) points-down) state-down)]
                                   [else
                                    (cons (+ (car prev-right) points-right) state-right)])])
                        (vector-set! (vector-ref cache m) n result)
                        result))))))))
      score))


  (let ([T (read)])
    (let ([M (make-vector T)]
          [N (make-vector T)])
      (for ([t (range T)])
        (vector-set! M t (read))
        (vector-set! N t (read)))
      (let ([score~ (make-score 
                     (vector-argmax values M) 
                     (vector-argmax values N))])
        (for ([t (range T)])
					; moving down 3 and right 3 always scores 21
					; and leaves the die in the same position as previously.
					; so we can always remove matching sets of 3 from M and N
					; and simply add 21 to the total.
					(let ([threes (quotient (min M N) 3)])
						(let ([M~ (- M (* 3 threes))]
						 			[N~ (- N (* 3 threes))])
							(let ([total (+ (* 21 threes)
							                (car (score~
							                           (sub1(vector-ref M~ t))
							                           (sub1(vector-ref N~ t)))))])
                (displayln total)))))))))

