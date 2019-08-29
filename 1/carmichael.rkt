#lang sicp
(define (square x) (* x x))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder
          (square (expmod base (/ exp 2) m))
          m))
        (else
         (remainder
          (* base (expmod base (- exp 1) m))
          m))))

(define (fast-prime? n a)
  (cond ((= a 0) true)
        ((= (expmod a n n) a) (fast-prime? n (- a 1)))
        (else false)))

(define (prime? n)
  (fast-prime? n (- n 1)))

(prime? 6601)
; 561, 1105, 1729, 2465, 2821, 6601