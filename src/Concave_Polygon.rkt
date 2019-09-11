#lang racket
; Enter your code here. Read input from STDIN. Print output to STDOUT


; given a set of points x, this function will
; calculate the perimeter length of a list of points z
; that define half of the convex hull
(define (hull x y z cmp)
    ; if there are no further points to add
    ; return the half-hull
    (if (zero? (length x))
        z
        ; if z is empty (the initial state)
        ; add the first two points from x
        (if (zero? (length z))
            (let* ([z~ (list (cadr x) (car x))])
                (hull (cddr x) (list) z~ cmp))
            ; if we have run out of line segments in z
            ; go on to the next candidate point in x
            (if (< (length z) 2)
                (let ([z~ (append (cons (car x) y) z)])
                    (hull (cdr x) (list) z~ cmp))
                ; otherwise test the next line segment in z
                ; against the candidate point
                (let* (
                    [p1  (cadr   z)]
                    [p2  (car    z)]
                    [p~  (car    x)]
                    [x1  (car   p1)]
                    [y1  (cadr  p1)]
                    [x2  (car   p2)]
                    [y2  (cadr  p2)]
                    [x~  (car   p~)]
                    [y~  (cadr  p~)])
                    ; if it passes, move on to the next segment 
                    ; if it fails, delete the line segment
                    (if (= x~ x2)
                        (if (cmp y2 y~)
                            (hull x (append y (list p2)) (cdr z) cmp)
                            (hull x y (cdr z) cmp))
                        (if (cmp y2 (+ y1 (* (- x2 x1) (/ (- y~ y1) (- x~ x1)))))
                            (hull x (append y (list p2)) (cdr z) cmp)
                            (hull x y (cdr z) cmp))))))))


; function to remove colinear points at extrema
(define (remove-colinear-extrema x)
    (let* (
        [minima_y
            (map
                cadr
                (filter
                    (lambda (i) (= (car i) (caar x)))
                    x))]
        [maxima_y
            (map
                cadr
                (filter
                    (lambda (i) (= (car i) (car (last x))))
                    x))])
        (filter
            (lambda (i)
                (not (or
                    (and
                        (= (car i) (caar x))
                        (> (cadr i) (foldl min +inf.0 minima_y))
                        (< (cadr i) (foldl max -inf.0 minima_y)))
                    (and
                        (= (car i) (car (last x)))
                        (> (cadr i) (foldl min +inf.0 maxima_y))
                        (< (cadr i) (foldl max -inf.0 maxima_y))))))
            x)))


; function to read data points
(define (read-pairs-len n)
    (if (= n 0)
        (list)
            (let* ([x (read)][y (read)])
                (cons (list x y) (read-pairs-len (- n 1))))))


; main
; read data, sort, translate, calculate and print output
(let* (
    [N (read)]
    [X (sort (read-pairs-len N) < #:key car)]
    [Y (remove-colinear-extrema X)]
    [H (append
        (cdr (hull Y (list) (list) <=))
        (cdr (reverse (hull Y (list) (list) >=))))])
    (if (> (length Y) (length H))
        (display "YES")
        (display "NO")))