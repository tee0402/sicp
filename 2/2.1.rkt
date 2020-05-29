#lang sicp

(define (numer x) (car x))
(define (denom x) (cdr x))
(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))
(define (make-rat n d)
  (let ((g (gcd n d)))
    (if (< d 0)
        (cons (/ (- n) g) (/ (- d) g))
        (cons (/ n g) (/ d g)))))
;(print-rat (make-rat 1 -2))
;(print-rat (make-rat -1 -2))

(define (average x y) (/ (+ x y) 2))
(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))
(define (make-segment start end) (cons start end))
(define (start-segment line) (car line))
(define (end-segment line) (cdr line))
(define (make-point x y) (cons x y))
(define (x-point p) (car p))
(define (y-point p) (cdr p))
(define (midpoint-segment line)
  (make-point
   (average (x-point (start-segment line)) (x-point (end-segment line)))
   (average (y-point (start-segment line)) (y-point (end-segment line)))))
(define a (make-segment (make-point 0 0) (make-point 6 0)))
;(print-point (midpoint-segment a))

(define (square x) (* x x))
(define (len line)
  (sqrt (+ (square (- (x-point (start-segment line)) (x-point (end-segment line))))
           (square (- (y-point (start-segment line)) (y-point (end-segment line)))))))
(define (make-rectangle s1 s2) (cons s1 s2))
;(define (make-rectangle p1 p2 p3 p4) (cons (cons p1 p2) (cons p3 p4)))
(define (length r) (len (car r)))
;(define (length r) (len (make-segment (car (car r)) (cdr (car r)))))
;(define (width r) (len (cdr r)))
;(define (width r) (len (make-segment (car (car r)) (cdr (cdr r)))))
(define (perimeter r)
  (* 2 (+ (length r) (width r))))
(define (area r)
  (* (length r) (width r)))
(define b (make-segment (make-point 0 0) (make-point 0 3)))
(define r (make-rectangle a b))
;(define r (make-rectangle (make-point 0 3) (make-point 6 3) (make-point 6 0) (make-point 0 0)))
;(perimeter r)
;(area r)

;(define (cons x y)
;  (lambda (m) (m x y)))
;(define (car z)
;  (z (lambda (p q) p)))
;(car (cons x y))
;(car (lambda (m) (m x y)))
;((lambda (m) (m x y)) (lambda (p q) p))
;((lambda (p q) p) x y)
;x
;(define (cdr z)
;  (z (lambda (p q) q)))

; If a is nonzero, then the product is divisible by 2
; If b is nonzero, then the product is divisible by 3
; To find a, just divide the product by 3 until it is not divisible by 3, then take the log base 2
; To find b, just divide the product by 2 until it is not divisible by 2, then take the log base 3
(define (lg b x) (/ (log x) (log b)))
;(define (cons a b) (* (expt 2 a) (expt 3 b)))
;(define (car z)
;  (if (> (remainder z 3) 0)
;      (lg 2 z)
;      (car (/ z 3))))
;(define (cdr z)
;  (if (> (remainder z 2) 0)
;      (lg 3 z)
;      (cdr (/ z 2))))

(define zero (lambda (f) (lambda (x) x)))
(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))
;(add-1 zero)
;(add-1 (lambda (f) (lambda (x) x)))
;(lambda (f) (lambda (x) (f (((lambda (f) (lambda (x) x)) f) x))))
;(lambda (f) (lambda (x) (f ((lambda (x) x) x))))
;(lambda (f) (lambda (x) (f x)))
(define one (lambda (f) (lambda (x) (f x))))
;(add-1 one)
;(add-1 (lambda (f) (lambda (x) (f x))))
;(lambda (f) (lambda (x) (f (((lambda (f) (lambda (x) (f x))) f) x))))
;(lambda (f) (lambda (x) (f ((lambda (x) (f x)) x))))
;(lambda (f) (lambda (x) (f (f x))))
(define two (lambda (f) (lambda (x) (f (f x)))))
;(define (+ a b)
;  (lambda (f) (lambda (x) ((b f) ((a f) x)))))

(define (lower-bound i) (car i))
(define (upper-bound i) (cdr i))

(define (make-interval a b) (cons a b))
; This could be implemented as (add-interval x -y)
; The minimum value is the lower bound of x minus the upper bound of y
; The maximum value is the upper bound of x minus the lower bound of y
(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))

;(define (width x) (/ (- (upper-bound x) (lower-bound x)) 2))
; We can show that for addition, the width of the result is equal to the sum of the widths of the two intervals:
;(= (+ (width x) (width y)) (width (add-interval x y)))
;(= (+ (/ (- (upper-bound x) (lower-bound x)) 2)
;      (/ (- (upper-bound y) (lower-bound y)) 2))
;   (width (make-interval (+ (lower-bound x) (lower-bound y))
;                         (+ (upper-bound x) (upper-bound y)))))
;(= (+ (/ (- (upper-bound x) (lower-bound x)) 2)
;      (/ (- (upper-bound y) (lower-bound y)) 2))
;   (/ (- (+ (upper-bound x) (upper-bound y))
;         (+ (lower-bound x) (lower-bound y))) 2))
;(ux-lx)/2+(uy-ly)/2 = ((ux+uy)-(lx+ly))/2
;                    = (ux-lx+uy-ly)/2
;                    = (ux-lx)/2+(uy-ly)/2 END
; For multiplication, we see that intervals with the same width can produce products with different widths:
; (1 3) * (1 3) = (1 9)
; width 1 * width 1 = width 4
; (2 4) * (2 4) = (4 16)
; width 1 * width 1 = width 6

(define (div-interval x y)
  (if (and (<= (lower-bound y) 0) (>= (upper-bound y) 0))
      (error "Dividing by interval spanning zero")
      (mul-interval
       x
       (make-interval (/ 1.0 (upper-bound y))
                      (/ 1.0 (lower-bound y))))))

; All cases: ++++,-+++,+-++,++-+,+++-,--++,-+-+,-++-,+--+,+-+-,++--,---+,--+-,-+--,+---,----
; Possible cases: ++++,-+++,++-+,--++,-+-+,++--,---+,-+--,----
(define (mul-interval x y)
  (let ((p1 (>= (lower-bound x) 0))
        (p2 (>= (upper-bound x) 0))
        (p3 (>= (lower-bound y) 0))
        (p4 (>= (upper-bound y) 0)))
    (cond ((and p1 p2 p3 p4)
           (make-interval (* (lower-bound x) (lower-bound y)) (* (upper-bound x) (upper-bound y))))
          ((and (not p1) p2 p3 p4)
           (make-interval (* (lower-bound x) (upper-bound y)) (* (upper-bound x) (upper-bound y))))
          ((and p1 p2 (not p3) p4)
           (make-interval (* (upper-bound x) (lower-bound y)) (* (upper-bound x) (upper-bound y))))
          ((and (not p1) (not p2) p3 p4)
           (make-interval (* (lower-bound x) (upper-bound y)) (* (upper-bound x) (lower-bound y))))
          ((and (not p1) p2 (not p3) p4)
           (make-interval (min (* (lower-bound x) (upper-bound y)) (* (upper-bound x) (lower-bound y)))
                          (max (* (lower-bound x) (lower-bound y)) (* (upper-bound x) (upper-bound y)))))
          ((and p1 p2 (not p3) (not p4))
           (make-interval (* (upper-bound x) (lower-bound y)) (* (lower-bound x) (upper-bound y))))
          ((and (not p1) (not p2) (not p3) p4)
           (make-interval (* (lower-bound x) (upper-bound y)) (* (lower-bound x) (lower-bound y))))
          ((and (not p1) p2 (not p3) (not p4))
           (make-interval (* (upper-bound x) (lower-bound y)) (* (lower-bound x) (lower-bound y))))
          ((and (not p1) (not p2) (not p3) (not p4))
           (make-interval (* (upper-bound x) (upper-bound y)) (* (lower-bound x) (lower-bound y)))))))

(define (make-center-percent c p)
  (make-interval (* c (- 1.0 (/ p 100.0))) (* c (+ 1.0 (/ p 100.0)))))
(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))
(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))
(define (percent i)
  (* (/ (width i) (center i)) 100))

; For small tolerances:
;(= (+ (percent x) (percent y)) (percent (mul-interval x y)))

; Every time an operation is performed on two intervals with uncertainty the uncertainty becomes larger (propagation of uncertainty/error),
; so the more operations you perform the larger the uncertainty
; par1 performs three operations that propagate error, while par2 only performs one, so par1 will give a larger uncertainty