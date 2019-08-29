#lang sicp

(define (map proc items)
  (if (null? items)
      nil
      (cons (proc (car items))
            (map proc (cdr items)))))

(define (square x) (* x x))
(define (square-list items)
  (if (null? items)
      nil
      (cons (square (car items)) (square-list (cdr items)))))
;(define (square-list items)
;  (map (lambda (x) (square x)) items))

;(square-list (list 1 2 3 4))

(define (for-each proc items)
  (cond ((null? items) true)
        (else (proc (car items)) (for-each proc (cdr items)))))

(for-each (lambda (x)
            (newline)
            (display x))
          (list 57 321 88))