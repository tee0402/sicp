#lang sicp

;(define (product term a next b)
;  (if (> a b)
;      1
;      (* (term a)
;         (product term (next a) next b))))

(define (product term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* result (term a)))))
  (iter a 1))

(define (identity x) x)
(define (inc x) (+ x 1))
(define (factorial n)
  (product identity 1 inc n))

(define (square x) (* x x))
(define (pi n)
  (define (pi-term x)
    (if (or (= x 2) (= x n)) x
        (square x)))
  (define (pi-inc x) (+ x 2))
  (* 4 (/ (product pi-term 2 pi-inc n) (product pi-term 3 pi-inc n))))

(pi 90)