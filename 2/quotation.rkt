#lang sicp

;(a b c)
;((george))
;((y1 y2))
;(y1 y2)
;false
;false
;(red shoes blue socks)

(define (equal? a b)
  (cond ((and (not (pair? a)) (not (pair? b))) (eq? a b))
        ((and (pair? a) (pair? b)) (and (equal? (car a) (car b)) (equal? (cdr a) (cdr b))))
        (else false)))

(equal? '(this (is a) list) '(this (is a) list))

;(car ''abracadabra)
;(car (quote (quote abracadabra)))