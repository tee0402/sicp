#lang sicp

(define (cube x) (* x x x))
;(define (sum term a next b)
;  (if (> a b)
;      0
;      (+ (term a)
;         (sum term (next a) next b))))
(define (simpson f a b n)
  (define (h)
    (/ (- b a) n))
  (define (next x)
      (+ x (h)))
  (define (simpson-f x)
    (cond ((or (= x a) (= x b)) (f x))
          ((odd? (/ (- x a) (h))) (* 4 (f x)))
          (else (* 2 (f x)))))
  (* (/ (h) 3) (sum simpson-f a next b)))
; 1/4
;(simpson cube 0 1 100)
; 1/4
;(simpson cube 0 1 1000)

;(define (sum term a next b)
;  (define (iter a result)
;    (if (> a b)
;        result
;        (iter (next a) (+ result (term a)))))
;  (iter a 0))

;(define (product term a next b)
;  (if (> a b)
;      1
;      (* (term a)
;         (product term (next a) next b))))
(define (identity x) x)
(define (inc x) (+ x 1))
(define (factorial n)
  (product identity 1 inc n))
;(factorial 5)

(define (square x) (* x x))
(define (pi n)
  (define (term x) (if (or (= x 2) (= x n)) x (square x)))
  (define (next x) (+ x 2))
  (* 4.0 (/ (product term 2 next n) (product term 3 next n))))
;(pi 10000)

;(define (product term a next b)
;  (define (iter a result)
;    (if (> a b)
;        result
;        (iter (next a) (* result (term a)))))
;  (iter a 1))

(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
                (accumulate combiner null-value term (next a) next b))))
(define (sum term a next b)
  (accumulate + 0 term a next b))
(define (product term a next b)
  (accumulate * 1 term a next b))

;(define (accumulate combiner null-value term a next b)
;  (define (iter a result)
;    (if (> a b)
;        result
;        (iter (next a) (combiner result (term a)))))
;  (iter a null-value))

(define (filtered-accumulate combiner null-value filter? term a next b)
  (if (> a b)
      null-value
      (combiner (if (filter? a) (term a) null-value)
                (filtered-accumulate combiner null-value filter? term (next a) next b))))

(define (divides? a b) (= (remainder b a) 0))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))
(define (smallest-divisor n) (find-divisor n 2))
(define (prime? n)
  (= n (smallest-divisor n)))
(define (sum-squared-primes a b)
  (filtered-accumulate + 0 prime? square a inc b))

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))
(define (relatively-prime-products n)
  (define (relatively-prime? a) (= (gcd n a) 1))
  (filtered-accumulate * 1 relatively-prime? identity 1 inc n))

;(f f)
;(f (lambda (g) (g 2)))
;((lambda (g) (g 2)) 2)
;(2 2)
; Error because 2 is not an operator

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
; phi^2 = phi + 1
; phi = 1 + 1/phi
;(fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0)

;(define (fixed-point f first-guess)
;  (define (close-enough? v1 v2)
;    (< (abs (- v1 v2))
;       tolerance))
;  (define (try guess)
;    (let ((next (f guess)))
;      (display guess)
;      (newline)
;      (if (close-enough? guess next)
;          next
;          (try next))))
;  (try first-guess))
(define (average a b) (/ (+ a b) 2))
; 34 steps without average damping
;(fixed-point (lambda (x) (/ (log 1000) (log x))) 2.0)
; 9 steps with average damping
;(fixed-point (lambda (x) (average x (/ (log 1000) (log x)))) 2.0)

(define (cont-frac n d k)
  (define (iter i result)
    (if (= i 0)
        result
        (iter (- i 1) (/ (n i) (+ (d i) result)))))
  (iter k 0))
; 1/phi = 0.61803398875
; k has to be 11 to be accurate to 4 decimal places
;(cont-frac (lambda (i) 1.0)
;           (lambda (i) 1.0)
;           11)

;(define (cont-frac n d k)
;  (define (recur n d i)
;    (if (= i (+ k 1))
;        0
;        (/ (n i) (+ (d i) (recur n d (+ i 1))))))
;  (recur n d 1))

; e = 2.71828
;(+ 2 (cont-frac (lambda (i) 1.0)
;                (lambda (i)
;                  (if (= (remainder (- i 2) 3) 0)
;                      (* 2.0 (+ (/ (- i 2) 3) 1))
;                      1.0))
;                20))

(define (tan-cf x k)
  (cont-frac (lambda (i) (if (= i 1) x (- (square x))))
             (lambda (i) (- (* i 2) 1))
             k))

(define (deriv g)
  (lambda (x) (/ (- (g (+ x dx)) (g x)) dx)))
(define dx 0.00001)
(define (newton-transform g)
  (lambda (x) (- x (/ (g x) ((deriv g) x)))))
(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))
(define (cubic a b c)
  (lambda (x) (+ (cube x) (* a (square x)) (* b x) c)))
;(newtons-method (cubic 1 2 2) 1)

(define (double f)
  (lambda (x) (f (f x))))
;(double (double double))
;(double (double (lambda (x) (f (f x)))))
;(double (lambda (x) ((lambda (x) (f (f x))) ((lambda (x) (f (f x))) x))))
;(double (lambda (x) ((lambda (x) (f (f x))) (f (f x)))))
;(double (lambda (x) (f (f (f (f x))))))
;(lambda (x) ((lambda (x) (f (f (f (f x))))) ((lambda (x) (f (f (f (f x))))) x)))
;(lambda (x) ((lambda (x) (f (f (f (f x))))) (f (f (f (f x))))))
;(f (f (f (f (f (f (f (f x))))))))
; This returns 13
;(((double (double double)) inc) 5)

(define (compose f g)
  (lambda (x) (f (g x))))
;((compose square inc) 6)

(define (repeated f n)
  (if (= n 1)
      f
      (compose f (repeated f (- n 1)))))
;((repeated square 2) 5)

(define (smooth f)
  (lambda (x) (/ (+ (f (- x dx)) (f x) (f (+ x dx))) 3)))
;((repeated smooth n) f)

(define (average-damp f)
  (lambda (x) (average x (f x))))
(define (floor-lg x) (floor (/ (log x) (log 2))))
(define (root x n)
  (fixed-point ((repeated average-damp (floor-lg n)) (lambda (y) (/ x (expt y (- n 1))))) 1.0))

(define (iterative-improve good-enough? improve)
  (define (iter guess)
    (if (good-enough? guess)
        guess
        (iter (improve guess))))
  iter)
(define (sqrt x)
  ((iterative-improve (lambda (guess) (< (abs (- (square guess) x)) 0.001))
                      (lambda (guess) (average guess (/ x guess))))
   x))
;(define (fixed-point f first-guess)
;  ((iterative-improve (lambda (guess) (< (abs (- guess (f guess))) tolerance))
;                      f)
;   first-guess))