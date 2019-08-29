#lang sicp

(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
                (accumulate combiner null-value term (next a) next b))))

;(define (accumulate combiner null-value term a next b)
;  (define (iter a result)
;    (if (> a b)
;        result
;        (iter (next a) (combiner result (term a)))))
;  (iter a null-value))

(define (sum term a next b)
  (accumulate + 0 term a next b))

(define (product term a next b)
  (accumulate * 1 term a next b))

(define (identity x) x)
(define (inc x) (+ x 1))

(define (filtered-accumulate combiner null-value filter? term a next b)
  (if (> a b)
      null-value
      (combiner (if (filter? a)
                    (term a)
                    null-value)
                (filtered-accumulate combiner null-value filter? term (next a) next b))))

;(define (square x) (* x x))
;(define (divides? a b) (= (remainder b a) 0))
;(define (find-divisor n test-divisor)
;  (cond ((> (square test-divisor) n) n)
;        ((divides? test-divisor n) test-divisor)
;        (else (find-divisor n (+ test-divisor 1)))))
;(define (smallest-divisor n) (find-divisor n 2))
;(define (prime? n)
;  (= n (smallest-divisor n)))

;(filtered-accumulate + 0 prime? square 1 inc 5)

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))
(define (relatively-prime? n)
  (define (relatively-prime a)
    (= (gcd a n) 1))
  relatively-prime)

(filtered-accumulate * 1 (relatively-prime? 6) identity 1 inc 6)