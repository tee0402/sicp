#lang sicp

;(define (up-split painter n)
;  (if (= n 0)
;      painter
;      (let ((smaller (up-split painter (- n 1))))
;        (below painter (beside smaller smaller)))))

(define (split big small)
  (define (recur painter n)
    (if (= n 0)
        painter
        (let ((smaller (recur painter (- n 1))))
          (big painter (small smaller smaller)))))
  recur)

;(define right-split (split beside below))
;(define up-split (split below beside))

(define (make-vect x y) (cons x y))
(define (xcor-vect v) (car v))
(define (ycor-vect v) (cdr v))
(define (add-vect v w)
  (make-vect (+ (xcor-vect v) (xcor-vect w)) (+ (ycor-vect v) (ycor-vect w))))
(define (sub-vect v w)
  (make-vect (- (xcor-vect v) (xcor-vect w)) (- (ycor-vect v) (ycor-vect w))))
(define (scale-vect s v)
  (make-vect (* s (xcor-vect v)) (* s (ycor-vect v))))

;(define (make-frame origin edge1 edge2) (list origin edge1 edge2))
;(define (origin-frame f) (car f))
;(define (edge1-frame f) (cadr f))
;(define (edge2-frame f) (caddr f))
(define (make-frame origin edge1 edge2) (cons origin (cons edge1 edge2)))
(define (origin-frame f) (car f))
(define (edge1-frame f) (cadr f))
(define (edge2-frame f) (cddr f))

(define (make-segment start end) (cons start end))
(define (start-segment s) (car s))
(define (end-segment s) (cdr s))

;(define outline
;  (segments->painter (list (make-segment (make-vect 0.0 0.0) (make-vect 0.0 1.0))
;                           (make-segment (make-vect 0.0 1.0) (make-vect 1.0 1.0))
;                           (make-segment (make-vect 1.0 1.0) (make-vect 1.0 0.0))
;                           (make-segment (make-vect 1.0 0.0) (make-vect 0.0 0.0)))))
;
;(define x
;  (segments->painter (list (make-segment (make-vect 0.0 0.0) (make-vect 1.0 1.0))
;                           (make-segment (make-vect 0.0 1.0) (make-vect 1.0 0.0)))))
;
;(define diamond
;  (segments->painter (list (make-segment (make-vect 0.0 0.5) (make-vect 0.5 1.0))
;                           (make-segment (make-vect 0.5 1.0) (make-vect 1.0 0.5))
;                           (make-segment (make-vect 1.0 0.5) (make-vect 0.5 0.0))
;                           (make-segment (make-vect 0.5 0.0) (make-vect 0.0 0.5)))))
;
;(define wave
;  (segments->painter (list (make-segment (make-vect 0.4 1.0) (make-vect 0.3 0.85))
;                           (make-segment (make-vect 0.3 0.85) (make-vect 0.4 0.7))
;                           (make-segment (make-vect 0.4 0.7) (make-vect 0.38 0.68))
;                           (make-segment (make-vect 0.38 0.68) (make-vect 0.3 0.7))
;                           (make-segment (make-vect 0.3 0.7) (make-vect 0.2 0.6))
;                           (make-segment (make-vect 0.2 0.6) (make-vect 0.0 0.75))
;                           
;                           (make-segment (make-vect 0.6 1.0) (make-vect 0.7 0.85))
;                           (make-segment (make-vect 0.7 0.85) (make-vect 0.6 0.7))
;                           (make-segment (make-vect 0.6 0.7) (make-vect 0.62 0.72))
;                           (make-segment (make-vect 0.62 0.72) (make-vect 0.7 0.7))
;                           (make-segment (make-vect 0.7 0.7) (make-vect 1.0 0.4))
;
;                           (make-segment (make-vect 0.45 0.85) (make-vect 0.5 0.8))
;                           (make-segment (make-vect 0.5 0.8) (make-vect 0.55 0.85))
;                           
;                           (make-segment (make-vect 0.0 0.65) (make-vect 0.2 0.5))
;                           (make-segment (make-vect 0.2 0.5) (make-vect 0.3 0.6))
;                           (make-segment (make-vect 0.3 0.6) (make-vect 0.2 0.0))
;                           
;                           (make-segment (make-vect 0.4 0.0) (make-vect 0.5 0.3))
;                           (make-segment (make-vect 0.5 0.3) (make-vect 0.6 0.0))
;                           
;                           (make-segment (make-vect 1.0 0.3) (make-vect 0.7 0.6))
;                           (make-segment (make-vect 0.7 0.6) (make-vect 0.8 0.0)))))

(define (flip-horiz painter)
  (transform-painter painter
                     (make-vect 1.0 0.0)
                     (make-vect 0.0 0.0)
                     (make-vect 1.0 1.0)))

(define (rotate180 painter)
  (transform-painter painter
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 1.0)
                     (make-vect 1.0 0.0)))

(define (rotate270 painter)
  (transform-painter painter
                     (make-vect 0.0 1.0)
                     (make-vect 0.0 0.0)
                     (make-vect 1.0 1.0)))

;(define (below painter1 painter2)
;  (let ((split-point (make-vect 0.0 0.5)))
;    (let ((paint-bottom (transform-painter painter1
;                                        (make-vect 0.0 0.0)
;                                        (make-vect 1.0 0.0)
;                                        split-point))
;          (paint-top (transform-painter painter2
;                                        split-point
;                                        (make-vect 1.0 0.5)
;                                        (make-vect 0.0 1.0))))
;      (lambda (frame)
;        (paint-bottom frame)
;        (paint-top frame)))))

(define (below painter1 painter2)
  (rotate90 (beside (rotate270 painter1) (rotate270 painter2))))

(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1))))
        (let ((corner (corner-split painter (- n 1))))
          (beside (below painter up)
                  (below right corner))))))

(define (square-limit painter n)
  (let ((combine4 (square-of-four flip-vert rotate180
                                  identity flip-horiz)))
    (combine4 (corner-split painter n))))