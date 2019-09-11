#lang racket

; Monotone Chain method

; function calculates convex hull from
; unsorted association list of points P
(define (convex-hull P)
    ;
    ; given a set of points P, this function will
    ; recursively build an association list of points H
    ; that define half of the convex hull
    (define (half-hull P H passed-H)
        ; if there are no further points to add
        ; report the half-hull except for the last point
        (if (zero? (length P))
            (cdr H)
            ; if H is empty (the initial state)
            ; add the first two points from P
            (if (zero? (length H))
                (let ([H~ (list (cadr P) (car P))])
                    (half-hull (cddr P) H~ (list)))
                ; if we have run out of line segments in H to test
                ; include the candidate point and
                ; move on to the next point in P
                (if (= 1 (length H))
                    (let ([H~ (append (cons (car P) passed-H) H)])
                        (half-hull (cdr P) H~ (list)))
                    ; otherwise test the next line segment in H (p1 p2)
                    ; against the candidate point p~
                    (let* (
                        [p1  (cadr H)]
                        [p2  (car  H)]
                        [p~  (car  P)]
                        [x1  (x    p1)]
                        [y1  (y    p1)]
                        [x2  (x    p2)]
                        [y2  (y    p2)]
                        [x~  (x    p~)]
                        [y~  (y    p~)])
                        ; if it passes, move on to the next segment 
                        ; if it fails, delete the line segment
                        (if
                            (> (* (- y2 y1) (- x~ x1)) (* (- x2 x1) (- y~ y1)))
                            (half-hull P (cdr H) (append passed-H (list p2)))
                            (half-hull P (cdr H) passed-H)))))))
    ;
    ; body of convex-hull
    (if
        (= 1 (length P))
        P
        (let ([P~ (sort (sort P > #:key cdr) > #:key car)])
            (append
                (half-hull P~ (list) (list))
                (half-hull (reverse P~) (list) (list))))))

; function to read data points into association list
(define (read-alist n)
    (if (= n 0)
        (list)
            (let* ([x (read)][y (read)])
                (cons (cons x y) (read-alist (- n 1))))))

; helper functions to extract point data
(define (x p) (car p))
(define (y p) (cdr p))

; calculate perimeter of convex hull H
(define (perimeter H acc)
    (if
        (pair? (cdr H))
        (let*
            (
                [p1 (car H)]
                [p2 (cadr H)]
                [x1 (x p1)]
                [y1 (y p1)]
                [x2 (x p2)]
                [y2 (y p2)]
                [dist
                    (sqrt
                        (+
                            (expt (- x1 x2) 2)
                            (expt (- y1 y2) 2)))])
            (perimeter (cdr H) (+ acc dist)))
            acc))

(let ([H (convex-hull (read-alist (read)))])
    (printf "~a~n" (perimeter (append (list (last H)) H) 0)))