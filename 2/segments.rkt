#lang sicp

(define (average x y) (/ (+ x y) 2))
(define (square x) (* x x))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(define (make-segment start end) (cons start end))
(define (start-segment line) (car line))
(define (end-segment line) (cdr line))

(define (make-point x y) (cons x y))
(define (x-point p) (car p))
(define (y-point p) (cdr p))

(define (midpoint-segment line)
  (make-point
   (average (x-point (start-segment line)) (x-point (end-segment line)))
   (average (y-point (start-segment line)) (y-point (end-segment line)))))

(define (length line)
  (sqrt (+ (square (- (x-point (start-segment line)) (x-point (end-segment line))))
           (square (- (y-point (start-segment line)) (y-point (end-segment line)))))))

(define line1 (make-segment (make-point 0 0) (make-point 0 5)))
(define line2 (make-segment (make-point 0 5) (make-point 5 5)))
;(print-point (midpoint-segment line1))

;(define (make-rectangle s1 s2) (cons s1 s2))
(define (make-rectangle p1 p2 p3 p4) (cons (cons p1 p2) (cons p3 p4)))
;(define (length1 r) (length (car r)))
(define (length1 r) (length (make-segment (car (car r)) (cdr (car r)))))
;(define (length2 r) (length (cdr r)))
(define (length2 r) (length (make-segment (cdr (car r)) (car (cdr r)))))

(define (perimeter r)
  (* 2 (+ (length1 r) (length2 r))))

(define (area r)
  (* (length1 r) (length2 r)))

;(define r (make-rectangle line1 line2))
(define r (make-rectangle (make-point 0 0) (make-point 0 5) (make-point 5 5) (make-point 5 0)))
(perimeter r)
(area r)