#lang sicp

(define (last-pair items)
  (let ((next (cdr items)))
    (if (null? next)
        (car items)
        (last-pair next))))

;(last-pair (list 23 72 149 34))

(define (reverse items)
  (define (iter items result)
    (if (null? items)
        result
        (iter (cdr items) (cons (car items) result))))
  (iter items nil))

;(reverse (list 1 4 9 16 25))

;(reverse (cons 1 (cons 4 (cons 9 (cons 16 (cons 25 nil))))))
;(cons 25 (cons 16 (cons 9 (cons 4 (cons 1 nil)))))

(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))

(define (no-more? coins)
  (null? coins))

(define (except-first-denomination coins)
  (cdr coins))

(define (first-denomination coins)
  (car coins))

(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
         (+ (cc amount
                (except-first-denomination
                 coin-values))
            (cc (- amount
                   (first-denomination
                    coin-values))
                coin-values)))))

;(cc 100 uk-coins)

(define (filter list filter?)
  (cond ((null? list) nil)
        ((filter? (car list)) (cons (car list) (filter (cdr list) filter?)))
        (else (filter (cdr list) filter?))))

(define (same-parity x . y)
  (if (odd? x)
      (cons x (filter y odd?))
      (cons x (filter y even?))))

(same-parity 1 2 3 4 5 6 7)
(same-parity 2 3 4 5 6 7)