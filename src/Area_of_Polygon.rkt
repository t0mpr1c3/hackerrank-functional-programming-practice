#lang racket
; Enter your code here. Read input from STDIN. Print output to STDOUT


; function calculates area of triangle
; arguments: p1 = (list x1 y1), p2 = (list x1 y1), 3rd point is origin = (list 0 0)
; area is negative when points oriented clockwise
(define (triangle p1 p2) 
    (let*
        (
            [x1 (car p1)]
            [x2 (car p2)]
            [y1 (cadr p1)]
            [y2 (cadr p2)])
        (* (- (* y2 x1) (* y1 x2)) 0.5)))

; function to calculate area of non-intersecting polygon
; assumes first and last points are (x, y) = (list 0 0)
; recursively calculate area of triangle made by first three points
; clockwise areas scored negative to handle non-convex polygons
; remove second point and repeat
; return sum of areas of triangles
(define (area x s)
    (let ([t (+ s (triangle (car x) (cadr x)))])
    (if
        (= (length x) 2)
        t
        (area (cdr x) t))))


; function to read data points
(define (read-pairs-len n)
    (if (= n 0)
        (list)
            (let* ([x (read)][y (read)])
                (cons (list x y) (read-pairs-len (- n 1))))))

; main
; read data, calculate and print output
(let*
    (
        [N (read)]
        [x (read-pairs-len N)]
        [i (caar x)]
        [j (cadar x)]
        [y
            (map
                (lambda (k)
                    (list
                        (- (car k) i)
                        (- (cadr k) j)))
                (cdr x))])
    (printf "~s\n" (area y 0)))