#lang sicp

;(define (+ a b)
;  (if (= a 0) b (inc (+ (dec a) b))))
;(+ 4 5)
;(inc (+ 3 5))
;(inc (inc (+ 2 5)))
;(inc (inc (inc (+ 1 5))))
;(inc (inc (inc (inc (+ 0 5)))))
;(inc (inc (inc (inc 5))))
;(inc (inc (inc 6)))
;(inc (inc 7))
;(inc 8)
;9
; Recursive

;(define (+ a b)
;  (if (= a 0) b (+ (dec a) (inc b))))
;(+ 4 5)
;(+ 3 6)
;(+ 2 7)
;(+ 1 8)
;(+ 0 9)
;9
; Iterative

;(define (A x y)
;  (cond ((= y 0) 0)
;        ((= x 0) (* 2 y))
;        ((= y 1) 2)
;        (else (A (- x 1) (A x (- y 1))))))
;(A 1 10)
;(A 0 (A 1 9))
;1024
;(A 2 4)
;(A 1 (A 2 3))
;(A 1 (A 1 (A 2 2)))
;(A 1 (A 1 (A 1 (A 2 1))))
;(A 1 (A 1 (A 1 2)))
;65536
;(A 3 3)
;65536

;2n
;(define (f n) (A 0 n))
;2^n
;(define (g n) (A 1 n))
;(2^2^2^2...)n times
;(define (h n) (A 2 n))

;(define (f n)
;  (if (< n 3) n (+ (f (- n 1)) (* 2 (f (- n 2))) (* 3 (f (- n 3))))))
;(define (f n)
;  (define (iter a b c count)
;    (if (= count 0) a (iter b c (+ (* 3 a) (* 2 b) c) (- count 1))))
; (iter 0 1 2 n))

(define (count-change amount) (cc amount 5))
(define (cc amount kinds-of-coins)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (= kinds-of-coins 0)) 0)
        (else (+ (cc amount
                     (- kinds-of-coins 1))
                 (cc (- amount
                        (first-denomination
                         kinds-of-coins))
                     kinds-of-coins)))))
(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
        ((= kinds-of-coins 2) 5)
        ((= kinds-of-coins 3) 10)
        ((= kinds-of-coins 4) 25)
        ((= kinds-of-coins 5) 50)))
; 1-4: 1s
; 5-9: 1s/one 5 and 1s
; 10-14: 1s/one 5 and 1s/two 5s and 1s/one 10 and 1s
; 15-19: 1s/one 5 and 1s/two 5s and 1s/three 5s and 1s/one 10 and 1s/one 10 and one 5 and 1s
; 20-24: 1s/one 5 and 1s/two 5s and 1s/three 5s and 1s/four 5s and 1s/one 10 and 1s/two 10 and 1s/one 10 and one 5 and 1s/one 10 and two 5s and 1s
;(count-change 20)

(define (pascal x y)
  (if (or (= y 0) (= x y)) 1 (+ (pascal (- x 1) (- y 1)) (pascal (- x 1) y))))
;(pascal 4 2)

;(p (sine 4.05))
;(p (p (sine 1.35)))
;(p (p (p (sine 0.45))))
;(p (p (p (p (sine 0.15)))))
;(p (p (p (p (p (sine 0.05))))))
; p is applied 5 times
; Space: theta(a)
; Steps: theta(log_3 a)

(define (square x) (* x x))
(define (fast-expt b n)
  (define (iter a base exp)
    (cond ((= exp 0) a)
          ((even? exp) (iter a (square base) (/ exp 2)))
          (else (iter (* a base) base (- exp 1)))))
  (iter 1 b n))
;(fast-expt 2 3)

(define (double x) (+ x x))
(define (halve x) (/ x 2))
; Recursive
;(define (fast-mult a b)
;  (cond ((= b 0) 0)
;        ((= b 1) a)
;        ((even? b) (double (fast-mult a (halve b))))
;        (else (+ a (fast-mult a (- b 1))))))

 ; Iterative: a + xy is invariant
(define (fast-mult a b)
  (define (iter z x y)
    (cond ((= y 0) z)
          ((even? y) (iter z (double x) (halve y)))
          (else (iter (+ z x) x (- y 1)))))
  (iter 0 a b))
;(fast-mult 4 5)

; a = bq+aq+ap
; b = bp+aq
; a <- (bp+aq)q+(bq+aq+ap)q+(bq+aq+ap)p = 2bpq+2aq^2+bq^2+2apq+ap^2 = b(2pq+q^2)+a(2pq+q^2)+a(p^2+q^2)
; b <- (bp+aq)p+(bq+aq+ap)q = bp^2+2apq+bq^2+aq^2 = b(p^2+q^2)+a(2pq+q^2)
(define (fib n)
  (fib-iter 1 0 0 1 n))
(define (fib-iter a b p q count)
  (cond ((= count 0) b)
        ((even? count)
         (fib-iter a
                   b
                   (+ (square p) (square q))
                   (+ (* 2 p q) (square q))
                   (/ count 2)))
        (else (fib-iter (+ (* b q) (* a q) (* a p))
                        (+ (* b p) (* a q))
                        p
                        q
                        (- count 1)))))
;(fib 8)

; Normal-order evaluation: 18 remainder calls
;(gcd 206 40)
;(gcd 40 (remainder 206 40))
; 1 in predicate: (remainder 206 40) = 6
;(gcd (remainder 206 40) (remainder 40 (remainder 206 40)))
; 2 in predicate: (remainder 40 (remainder 206 40)) = 4
;(gcd (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40))))
; 4 in predicate: (remainder (remainder 206 40) (remainder 40 (remainder 206 40))) = 2
;(gcd (remainder (remainder 206 40) (remainder 40 (remainder 206 40))) (remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))))
; 7 in predicate: (remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))) = 0
; 4 in consequent: (remainder (remainder 206 40) (remainder 40 (remainder 206 40))) = 2
;2

; Applicative-order evaluation: 4 remainder calls
;(gcd 206 40)
;(gcd 40 (remainder 206 40))
;(gcd 40 6)
;(gcd 6 (remainder 40 6))
;(gcd 6 4)
;(gcd 4 (remainder 6 4))
;(gcd 4 2)
;(gcd 2 (remainder 4 2))
;(gcd 2 0)
;2

(define (smallest-divisor n) (find-divisor n 2))
;(define (find-divisor n test-divisor)
;  (cond ((> (square test-divisor) n) n)
;        ((divides? test-divisor n) test-divisor)
;        (else (find-divisor n (+ test-divisor 1)))))
(define (divides? a b) (= (remainder b a) 0))

; 199
;(smallest-divisor 199)
; 1999
;(smallest-divisor 1999)
; 7
;(smallest-divisor 19999)

(define (prime? n)
  (= n (smallest-divisor n)))
(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))
;(define (start-prime-test n start-time)
;  (if (prime? n)
;      (report-prime (- (runtime) start-time))))
(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(define (search-for-primes a b)
  (cond ((> a b) (newline)
                 (display "done"))
        ((even? a) (search-for-primes (+ a 1) b))
        (else (timed-prime-test a)
              (search-for-primes (+ a 2) b))))
; 1009, 1013, 1019
;(search-for-primes 1000 1020)
; 10007, 10009, 10037
;(search-for-primes 10000 10040)
; 100003, 100019, 100043
;(search-for-primes 100000 100050)
; 1000003, 1000033, 1000037
;(search-for-primes 1000000 1000040)

(define (next n)
  (if (= n 2) 3 (+ n 2)))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor)))))

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
(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))
(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))
(define (start-prime-test n start-time)
  (if (fast-prime? n 1000)
      (report-prime (- (runtime) start-time))))

;(define (expmod base exp m)
;  (remainder (fast-expt base exp) m))
;(expmod 98765 1234567 1234567)
; The procedure works but it is very resource intensive for large exponents because we have to multiply very large numbers together

; Instead of taking advantage of applicative-order evaluation, Louis forces the evaluation of square to be normal-order evaluation.
; Therefore, every time the exponent is halved, the number of calls to expmod doubles, effectively negating the logarithmic speed
; advantage gained from the halving of the problem

(define (carmichael-test n)
  (define (try-it a)
    (cond ((= a 0) true)
          ((= (expmod a n n) a) (try-it (- a 1)))
          (else false)))
  (try-it (- n 1)))
;(carmichael-test 561)
;(carmichael-test 1105)
;(carmichael-test 1729)
;(carmichael-test 2465)
;(carmichael-test 2821)
;(carmichael-test 6601)

(define (miller-rabin-expmod base exp m)
  (define (remainder-square expmod-reduced)
    (define (check sqrt-check)
      (if (and (not (= expmod-reduced 1)) (not (= expmod-reduced (- m 1))) (= sqrt-check 1)) 0 sqrt-check))
    ; Make use of applicative-order evaluation to avoid computing remainder twice
    (check (remainder (square expmod-reduced) m)))
  (cond ((= exp 0) 1)
        ; Make use of applicative-order evaluation to avoid computing expmod twice
        ((even? exp) (remainder-square (miller-rabin-expmod base (/ exp 2) m)))
        (else
         (remainder
          (* base (miller-rabin-expmod base (- exp 1) m))
          m))))
(define (miller-rabin-test n)
  (define (try-it a)
    (= (miller-rabin-expmod a (- n 1) n) 1))
  (try-it (+ 1 (random (- n 1)))))
(define (miller-rabin-prime? n times)
  (cond ((= times 0) true)
        ((miller-rabin-test n) (miller-rabin-prime? n (- times 1)))
        (else false)))
;(miller-rabin-prime? 1009 100)
;(miller-rabin-prime? 1013 100)
;(miller-rabin-prime? 561 100)
;(miller-rabin-prime? 1105 100)
;(miller-rabin-prime? 1729 100)
;(miller-rabin-prime? 2465 100)
;(miller-rabin-prime? 2821 100)
;(miller-rabin-prime? 6601 100)