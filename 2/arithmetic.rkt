#lang sicp

; magnitude calls apply-generic, which strips off the complex tag and calls magnitude again, which calls
; apply-generic, which strips off the rectangular tag and calls the magnitude procedure for the rectangular
; representation, returning 5

(define (attach-tag type-tag contents)
  (if (eq? type-tag 'scheme-number)
      contents
      (cons type-tag contents)))
(define (type-tag datum)
  (cond ((pair? datum) (car datum))
        ((number? datum) 'scheme-number)
        (else (error "Bad tagged datum: TYPE-TAG" datum))))
(define (contents datum)
  (cond ((pair? datum) (cdr datum))
        ((number? datum) datum)
        (else (error "Bad tagged datum: CONTENTS" datum))))

(define (equ? x y)
  (= x y))
(define (=zero? x)
  (= x 0))
(put 'equ? '(scheme-number scheme-number)
     (lambda (x y) (tag (equ? x y))))
(put '=zero? '(scheme-number scheme-number)
     (lambda (x) (tag (=zero? x))))

(define (equ? x y)
  (and (= (numer x) (numer y)) (= (denom x) (denom y))))
(define (=zero? x)
  (= (numer x) 0))
(put 'equ? '(rational rational)
     (lambda (x y) (tag (equ? x y))))
(put '=zero? '(rational rational)
     (lambda (x) (tag (=zero? x))))

(define (equ? x y)
  (and (= (real-part x) (real-part y)) (= (imag-part x) (imag-part y))))
(define (=zero? x)
  (= (magnitude x) 0))
(put 'equ? '(complex complex)
     (lambda (x y) (tag (equ? x y))))
(put '=zero? '(complex complex)
     (lambda (x) (tag (=zero? x))))