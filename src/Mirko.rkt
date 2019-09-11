#lang racket
; Enter your code here. Read input from STDIN. Print output to STDOUT

; building struct
; intercept: initial height
; slope: height added per day
; index: 1-index
(struct building (intercept slope index))
(define (building->string b)
  (string-append* (cdr (append*
                        (map (lambda (j) (list ", " (number->string j)))
                             (list (building-intercept b)
                                   (building-slope b)
                                   (building-index b)))))))

; crossing points struct
(struct xpoint (building day))
    
; day struct
(struct day (n result index))
(define (day->string d)
  (string-append* (cdr (append*
                        (map (lambda (j) (list ", " (number->string j)))
                             (list (day-n d)
                                   (day-result d)
                                   (day-index d)))))))

(let* ([N (read)]
       [Q (read)]
       [X (list->vector (for/list ([i (range N)]) (values (read))))]
       [Y (list->vector (for/list ([i (range N)]) (values (read))))]
       [Z (list->vector (for/list ([i (range Q)]) (values (read))))])
  (let ([B (make-vector N (building 0 0 0))]
        [D (make-vector Q (day 0 0 0))])
    (for ([i (range N)])
      (vector-set! B i (building (vector-ref X i) (vector-ref Y i) (add1 i))))
    (for ([i (range Q)])
      (vector-set! D i (day (vector-ref Z i) 0 i)))
    ;
    ; first sort buildings by their y-value
    ; and remove any with lower x or i values
    (let* ([B~ (vector-sort B < #:key building-slope)]
           [F (let add-building ([i (sub1 N)]
                                 [f (list)])
                (if (< i 0)
                    f
                    (let ([b (vector-ref B~ i)])
                      (if (empty? f)
                          (add-building (sub1 i) (list b))
                          (if (> (building-intercept b) (building-intercept (car f)))
                              (if (= (building-slope b) (building-slope (car f)))
                                  (add-building (sub1 i) (cons b (cdr f)))
                                  (add-building (sub1 i) (cons b f)))
                              (add-building (sub1 i) f))))))]
           [F~ (list->vector (reverse F))]
           [N~ (vector-length F~)])
      ;
      ; working from high x/low y to low x/high y,
      ; that is to say in order of increasing slope,
      ; calculate crossing times between successive lines
      ; removing any that fall out of sequence
      (let ([P (let xpoints ([f F][b '()])
                 (if (empty? f)
                     (reverse b)
                     (if (empty? b)
                         (xpoints (cdr f) (list (xpoint (car f) 0)))
                         (let ([b1 (xpoint-building (car b))]
                               [d1 (xpoint-day (car b))]
                               [b2 (car f)])
                           (let ([d2 (/ (- (building-intercept b2) (building-intercept b1) 0.0)
                                        (- (building-slope b1) (building-slope b2)))])
                             (if (< d1 d2)
                                 (xpoints (cdr f) (cons (xpoint b2 d2) b))
                                 (xpoints f (cdr b))))))))])
        ;
        ; perform tests in order of increasing day
        ; check buildings in order
        ; working from high x/low y to low x/high y,
        ; that is to say in order of increasing slope,
        ; popping them off the list
        ; once their crossing point has passed
        (let ([D~ (vector->list (vector-sort D < #:key day-n))])
          (let tallest ([d D~]
                        [p P])
            (when (and (not (empty? d))
                       (not (empty? d)))
              (let ([p1 (car p)]
                    [p~ (car d)])
                (let ([d1 (xpoint-day p1)]
                      [d~ (day-n p~)])
                  (if (> d1 d~)
                      (error "this should not happen")
                      (if (= (length p) 1)
                          (begin
                            (vector-set! D
                                         (day-index p~)
                                         (day d~
                                              (building-index (xpoint-building p1))
                                              (day-index p~)))
                            (tallest (cdr d) p))
                          (let ([p2 (cadr p)])
                            (let ([d2 (xpoint-day p2)])
                              (if (or (< d~ d2)
                                      (and (= d~ d2)
                                           (< (building-index (xpoint-building p2))
                                              (building-index (xpoint-building p1)))))
                                  (begin
                                    (vector-set! D
                                                 (day-index p~)
                                                 (day d~
                                                      (building-index (xpoint-building p1))
                                                      (day-index p~)))
                                    (tallest (cdr d) p))
                                  (tallest d (cdr p)))))))))))
          ;
          (for ([d D])
            (printf "~a~n" (day-result d))))))))
