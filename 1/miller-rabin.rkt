#lang sicp

(define (square x) (* x x))

(define (expmod base exp m)
  ; We need to use a variable abstraction to prevent computing expmod multiple times
  ; One way to do this is with a procedure parameter, which is a local variable
  (define (miller-rabin-square x)
    ; Use another procedure abstraction since we need to use (remainder (square x) m) twice
    (define (check square-mod)
      (if (and (not (= x 1)) (not (= x (- m 1))) (= square-mod 1)) 0
          square-mod))
    (check (remainder (square x) m)))
  (cond ((= exp 0) 1)
        ((even? exp) (miller-rabin-square (expmod base (/ exp 2) m)))
        (else
         (remainder
          (* base (expmod base (- exp 1) m))
          m))))

(define (miller-rabin-test n)
  (define (try-it a)
    (= (expmod a (- n 1) n) 1))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((miller-rabin-test n) (fast-prime? n (- times 1)))
        (else false)))

(define (prime? n)
  (if (even? n) false
      (fast-prime? n (+ (quotient n 2) 1))))

(prime? 3331)
; 561, 1105, 1729, 2465, 2821, 6601
;(expmod 2 2 3)
;(remainder (rabin-miller-square (expmod 2 (/ 2 2) 3)) 3)
;(remainder (rabin-miller-square (expmod 2 1 3)) 3)
;(remainder (rabin-miller-square (remainder (* 2 (expmod 2 (- 1 1) 3)) 3)) 3)
;(remainder (rabin-miller-square (remainder (* 2 (expmod 2 0 3)) 3)) 3)
;(remainder (rabin-miller-square (remainder (* 2 1) 3)) 3)
;(remainder (rabin-miller-square (remainder 2 3)) 3)
;(remainder (rabin-miller-square 2) 3)
;(remainder (check 2 (remainder (square 2) 3)) 3)
;(remainder (check 2 (remainder (* 2 2) 3)) 3)
;(remainder (check 2 (remainder 4 3)) 3)
;(remainder (check 2 1) 3)
;(remainder 1 3)
;1