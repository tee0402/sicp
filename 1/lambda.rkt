#lang sicp

(define (double f)
  (lambda (x) (f (f x))))

;((double inc) 1)
;(((double (double double)) inc) 5)

(define (square x) (* x x))
(define (compose f g)
  (lambda (x) (f (g x))))

;((compose square inc) 6)

(define (repeated f n)
  (if (= n 1)
      (lambda (x) (f x))
      (repeated (compose f f) (- n 1))))

;((repeated square 2) 5)

(define dx 0.00001)
(define (smooth f)
  (lambda (x) (/ (+ (f (- x dx)) (f x) (f (+ x dx))) 3)))

(((repeated smooth 3) square) 3)