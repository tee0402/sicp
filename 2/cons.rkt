#lang sicp

;(define (cons x y)
;  (lambda (m) (m x y)))
;(define (car z)
;  (z (lambda (p q) p)))
;(define (cdr z)
;  (z (lambda (p q) q)))

;(cdr (cons 1 2))

(define (square x) (* x x))
(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))

(define (lg b x) (/ (log x) (log b)))

(define (cons a b) (* (fast-expt 2 a) (fast-expt 3 b)))
(define (car z)
  (if (> (remainder z 3) 0)
      (lg 2 z)
      (car (/ z 3))))
(define (cdr z)
  (if (> (remainder z 2) 0)
      (lg 3 z)
      (cdr (/ z 2))))

(cdr (cons 3 4))