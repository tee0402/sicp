#lang sicp

(define (average a b) (/ (+ a b) 2))

(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2))
       tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (display guess)
      (newline)
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

;(fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0)
;(fixed-point (lambda (x) (average x (/ (log 1000) (log x)))) 2.0)

;(define (cont-frac n d k)
;  (define (iter i result)
;    (if (= i 0)
;        result
;        (iter (- i 1) (/ (n i) (+ (d i) result)))))
;  (iter k 0))

(define (cont-frac n d k)
  (define (recur n d i)
    (if (= i (+ k 1))
        0
        (/ (n i) (+ (d i) (recur n d (+ i 1))))))
  (recur n d 1))

; 1 / phi
;(cont-frac (lambda (i) 1.0)
;           (lambda (i) 1.0)
;           k)

; e
;(+ 2 (cont-frac (lambda (i) 1.0)
;                (lambda (i)
;                  (if (= (remainder (- i 2) 3) 0)
;                      (* 2 (+ (/ (- i 2) 3) 1))
;                      1.0))
;                20))

(define (square x) (* x x))
(define (tan-cf x k)
  (cont-frac (lambda (i)
               (if (= i 1) x
                   (- (square x))))
             (lambda (i) (- (* i 2) 1))
             k))

(tan-cf 3.14 20)