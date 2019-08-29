#lang sicp

(define (append! x y)
  (set-cdr! (last-pair x) y)
  x)
(define (last-pair x)
  (if (null? (cdr x)) x (last-pair (cdr x))))

;(define x (list 'a 'b))
;(define y (list 'c 'd))
;(define z (append x y))
;z
;(cdr x)
;(define w (append! x y))
;w
;(cdr x)

; reverse
(define (mystery x)
  (define (loop x y)
    (if (null? x)
        y
        (let ((temp (cdr x)))
          (set-cdr! x y)
          (loop temp x))))
  (loop x '()))

;(define v (list 'a 'b 'c 'd))
;(define w (mystery v))
;v
;w

(define (in item x)
  (cond ((null? x) false)
        ((eq? item (car x)) true)
        (else (in item (cdr x)))))

(define (count-pairs x)
  (define counted '())
  (define (recur x)
    (if (or (not (pair? x)) (in x counted))
        0
        (begin (set! counted (cons x counted))
               (+ (recur (car x))
                  (recur (cdr x))
                  1))))
  (recur x))

;(define x (list 'a))
;(define y (cons x x))
;(count-pairs (cons y y))

(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

(define z (make-cycle (list 'a 'b 'c)))

;(define (check-cycle z)
;  (define visited '())
;  (define (iter x)
;    (cond ((null? x) false)
;          ((in x visited) true)
;          (else (set! visited (cons x visited))
;                (iter (cdr x)))))
;  (iter z))

(define (contains x iter-list limit)
  (cond ((= limit 0) false)
        ((eq? x iter-list) true)
        (else (contains x (cdr iter-list) (- limit 1)))))

(define (check-cycle z)
  (define (iter x limit)
    (cond ((null? x) false)
          ((contains x z limit) true)
          (else (iter (cdr x) (+ limit 1)))))
  (iter z 0))

(check-cycle z)