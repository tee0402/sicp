#lang sicp

(define (square x) (* x x))
(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))

(define (compose f g)
  (lambda (x) (f (g x))))
(define (repeated f n)
  (if (= n 1)
      (lambda (x) (f x))
      (repeated (compose f f) (- n 1))))
(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2))
       tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (average x y) (/ (+ x y) 2))
(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (lg x) (floor (/ (log x) (log 2))))

(define (root n x)
  (fixed-point ((repeated average-damp (lg n)) (lambda (y) (/ x (fast-expt y (- n 1))))) 1.0))

(root 86 35412)

; 1: 1-2
; 2: 4-6
; 3: 7-30
; 4: 31-4865
; 5: 1666924
; 3 7 31 4866