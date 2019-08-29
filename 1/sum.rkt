#lang sicp

(define (cube x) (* x x x))

;(define (sum term a next b)
;  (if (> a b)
;      0
;      (+ (term a)
;         (sum term (next a) next b))))

(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ result (term a)))))
  (iter a 0))

(define (simpson f a b n)
  (define (h)
    (/ (- b a) n))
  (define (simpson-next x)
      (+ x (h)))
  (define (simpson-f x)
    (cond ((or (= x a) (= x (+ a (* n (h))))) (f x))
          ((odd? (/ (- x a) (h))) (* 4 (f x)))
          (else (* 2 (f x)))))
  (* (/ (h) 3) (sum simpson-f a simpson-next (+ a (* n (h))))))

(simpson cube 0 1 4)
; h = 1/4
;(sum simpson-f 0 simpson-next 1)
;(+ 0 (sum simpson-f 1/4 simpson-next 1))
;(+ 0 (+ 4/64 (sum simpson-f 2/4 simpson-next 1)))
;(+ 0 (+ 4/64 (+ 16/64 (sum simpson-f 3/4 simpson-next 1))))
;(+ 0 (+ 4/64 (+ 16/64 (+ 108/64 (sum simpson-f 1 simpson-next 1)))))
;(+ 0 (+ 4/64 (+ 16/64 (+ 108/64 (+ 1 (sum simpson-f 5/4 simpson-next 1))))))
;(+ 0 (+ 4/64 (+ 16/64 (+ 108/64 (+ 1 0)))))
; 1/12(4/64 + 16/64 + 108/64 + 1) = 3/12 = 1/4