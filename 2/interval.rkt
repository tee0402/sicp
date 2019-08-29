#lang sicp

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

; ++++,-+++,+-++,++-+,+++-,--++,-+-+,-++-,+--+,+-+-,++--,---+,--+-,-+--,+---,----
; ++++,-+++,++-+,--++,-+-+,++--,---+,-+--,----
;(define (mul-interval x y)
;  (let ((p1 (* (lower-bound x) (lower-bound y)))
;        (p2 (* (lower-bound x) (upper-bound y)))
;        (p3 (* (upper-bound x) (lower-bound y)))
;        (p4 (* (upper-bound x) (upper-bound y))))
;    (make-interval (min p1 p2 p3 p4)
;                   (max p1 p2 p3 p4))))

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

(define (div-interval x y)
  (if (and (<= (lower-bound y) 0) (>= (upper-bound y) 0))
      (error "Dividing by interval spanning zero")
      (mul-interval
       x
       (make-interval (/ 1.0 (upper-bound y))
                      (/ 1.0 (lower-bound y))))))

(define (make-interval a b) (cons a b))
(define (lower-bound i) (car i))
(define (upper-bound i) (cdr i))

(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))

(define (width x) (/ (- (upper-bound x) (lower-bound x)) 2))

(define x (make-interval 1 3))
(define y (make-interval 1 3))
;(= (+ (width x) (width y)) (width (add-interval x y)))
;(= (+ (/ (- (upper-bound x) (lower-bound x)) 2)
;      (/ (- (upper-bound y) (lower-bound y)) 2))
;   (width (make-interval (+ (lower-bound x) (lower-bound y))
;                         (+ (upper-bound x) (upper-bound y)))))
;(= (+ (/ (- (upper-bound x) (lower-bound x)) 2)
;      (/ (- (upper-bound y) (lower-bound y)) 2))
;   (/ (- (+ (upper-bound x) (upper-bound y))
;         (+ (lower-bound x) (lower-bound y))) 2))
;(ux-lx)/2+(uy-ly)/2=((ux+uy)-(lx+ly))/2
;4 4 14
;5 5 20
;(mul-interval x y)

(define (make-center-percent c p)
  (make-interval (* c (- 1.0 (/ p 100.0))) (* c (+ 1.0 (/ p 100.0)))))
(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))
(define (percent i)
  (* (/ (width i) (center i)) 100))

;(define c1 10)
;(define c2 10)
;(define p1 10)
;(define p2 10)
;(percent (mul-interval (make-center-percent c1 p1) (make-center-percent c2 p2)))
;(percent
; (make-interval
;  (* (* c1 (- 1.0 (/ p1 100.0))) (* c2 (- 1.0 (/ p2 100.0))))
;  (* (* c1 (+ 1.0 (/ p1 100.0))) (* c2 (+ 1.0 (/ p2 100.0))))))
;(*
; (/
;    (/ (- (* (* c1 (+ 1.0 (/ p1 100.0))) (* c2 (+ 1.0 (/ p2 100.0))))
;          (* (* c1 (- 1.0 (/ p1 100.0))) (* c2 (- 1.0 (/ p2 100.0))))) 2)
;    (/ (+ (* (* c1 (- 1.0 (/ p1 100.0))) (* c2 (- 1.0 (/ p2 100.0))))
;          (* (* c1 (+ 1.0 (/ p1 100.0))) (* c2 (+ 1.0 (/ p2 100.0))))) 2))
; 100)
; For small tolerances
;(= (+ (percent x) (percent y)) (percent (mul-interval x y)))

(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))
(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval
     one (add-interval (div-interval one r1)
                       (div-interval one r2)))))

(percent (par2 x y))