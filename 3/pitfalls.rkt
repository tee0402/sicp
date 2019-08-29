#lang sicp

(define (make-account balance password)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        (error "Insufficient funds")))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (lambda (p m)
    (if (eq? p password)
        (cond ((eq? m 'withdraw) withdraw)
              ((eq? m 'deposit) deposit)
              (else (error "Unknown request: MAKE-ACCOUNT"
                           m)))
        (error "Incorrect password"))))

(define (make-joint account old-password new-password)
  (account old-password 'withdraw)
  (lambda (p m)
    (if (eq? p new-password)
        (cond ((eq? m 'withdraw) (account old-password 'withdraw))
              ((eq? m 'deposit) (account old-password 'deposit))
              (else (error "Unknown request: MAKE-ACCOUNT"
                           m)))
        (error "Incorrect password"))))

(define peter-acc (make-account 50 'open-sesame))
(define paul-acc (make-joint peter-acc 'open-sesame 'rosebud))

;((peter-acc 'open-sesame 'withdraw) 10)
;((paul-acc 'rosebud 'withdraw) 10)

(define f
  (let ((a 1))
    (lambda (x) (set! a (* a x)) a)))

;(+ (f 0) (f 1))
;(+ (f 1) (f 0))