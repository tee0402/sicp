#lang sicp

; 123,132,213,231,312,321
; If forced to run sequentially, transactions can be reordered
; 123,213:45 ; 132:35 ; 231:50 ; 312,321:40
; If interleaved, transactions can be reordered and lost
; 1:110 ; 2:80 ; 3:50 ; 12,21:90 ; 13:55 ; 23:40, 31:60 ; 32:30

; 101, 121, 100

; 1000000: P1 then P2; P2 then P1
; 10000: First P1 access, P2, second P1 access; First and second P2 accesses, P1, third P2 access
; 100000: First P2 access, P1, second and third P2 accesses
; 100: P1 accesses, P2, P1 sets
; 1000: P2 accesses, P1, P2 sets
; Only 1000000 remains if serialized

; No because access is a single operation. Since the withdraw and deposit procedures both only have a single set, interleaving access
; within them will produce results that are the same as if the procedures were run sequentially

; Assuming that the serializer returns the same procedure during each call with the same procedure as the parameter, it is a safe change
; to make because then there would be no difference between the two versions

; If the exchanges are run sequentially, then the order of the balances is simply rearranged during each exchange
; The sum of the balances will be preserved because whatever we withdraw from one account, we always deposit back the same amount into
; another account

; There is no problem because the access and set! operations on each individual balance are contained within the serialized deposit
; and withdraw procedures, unlike in the exchange problem. This means that between the time when a balance is accessed and when a balance is
; set based on that accessed value the balance cannot have changed. That leaves the problem of not having enough money in the from-account
; to withdraw because another transfer occurred before the current transfer, which we assume is not a problem

; Louis is wrong because the serialized exchange would then call a withdraw in the same serialized set, which would cause a deadlock because
; the withdraw would need to wait for the exchange to finish first, but the exchange was the one that called withdraw, so it is waiting for
; the withdraw to finish

;(define (make-mutex)
;  (let ((cell (list false)))
;    (define (the-mutex m)
;      (cond ((eq? m 'acquire)
;             (if (test-and-set! cell)
;                 (the-mutex 'acquire))) ; retry
;            ((eq? m 'release) (clear! cell))))
;    the-mutex))
;(define (clear! cell) (set-car! cell false))
;(define (test-and-set! cell)
;  (if (car cell) true (begin (set-car! cell true) false)))

;(define (make-semaphore n)
;  (let ((semaphore (make-mutex-list n)))
;    (define (the-semaphore m)
;      (cond ((eq? m 'acquire)
;             (if (acquire semaphore)
;                 (the-semaphore 'acquire)))
;            ((eq? m 'release) (release semaphore))))
;    the-semaphore))
;(define (make-node item prev next) (list item prev next))
;(define (item node) (car node))
;(define (prev node) (cadr node))
;(define (next node) (caddr node))
;(define (set-item! node item) (set-car! node item))
;(define (set-prev! node prev) (set-car! (cdr node) prev))
;(define (set-next! node next) (set-car! (cddr node) next))
;(define (make-dll) (list (make-node '() '() '())))
;(define (front dll) (car dll))
;(define (set-front! dll front) (set-car! dll front))
;(define (insert-dll! dll item)
;  (let ((new-node (make-node item (front dll) '())))
;    (set-next! (front dll) new-node)
;    (set-front! dll new-node)))
;(define (make-mutex-list n)
;  (define (iter n dll)
;    (if (= n 0)
;        dll
;        (begin (insert-dll! dll (make-mutex))
;               (iter (- n 1) dll))))
;  (iter n (make-dll)))
;(define (all-acquired? semaphore) (null? (item (front semaphore))))
;(define (acquire semaphore)
;  (if (all-acquired? semaphore)
;      true
;      (begin ((item (front semaphore)) 'acquire)
;             (set-front! semaphore (prev (front semaphore)))
;             false)))
;(define (none-acquired? semaphore) (null? (next (front semaphore))))
;(define (release semaphore)
;  (if (not (none-acquired? semaphore))
;      (begin (set-front! semaphore (next (front semaphore)))
;             ((item (front semaphore)) 'release))))
;(define sema (make-semaphore 1))
;(sema 'acquire)
;(sema 'release)
;(sema 'acquire)

(define (make-semaphore n)
  (let ((semaphore (list 0)))
    (define (the-semaphore m)
      (cond ((eq? m 'acquire)
             (if (test-and-set! semaphore n)
                 (the-semaphore 'acquire)))
            ((eq? m 'release) (clear! semaphore))))
    the-semaphore))
(define (clear! semaphore) (if (> (car semaphore) 0) (set-car! semaphore (- (car semaphore) 1))))
(define (test-and-set! semaphore n)
  (if (= (car semaphore) n) true (begin (set-car! semaphore (+ (car semaphore) 1)) false)))

; The deadlock-avoidance method makes the resource acquisition process linear and eliminates cycles. Every time a
; process is blocked, it is blocked by a process that requires resources in the latter part of the chain, so that process can finish
; without needing the blocked process to finish first, unless it is itself blocked, in which case we simply go down the chain and find the
; process furthest down the chain, which is not blocked and can clear the blockage one by one once it finishes
(define make-id
  (let ((id 0))
    (lambda ()
      (set! id (+ id 1))
      id)))
(define (make-account-and-serializer balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (let ((id (make-id))
        (balance-serializer (make-serializer)))
    (define (dispatch m)
      (cond ((eq? m 'withdraw) withdraw)
            ((eq? m 'deposit) deposit)
            ((eq? m 'balance) balance)
            ((eq? m 'id) id)
            ((eq? m 'serializer) balance-serializer)
            (else (error "Unknown request: MAKE-ACCOUNT" m))))
    dispatch))
(define (serialized-exchange account1 account2)
  (let ((id1 (account1 'id))
        (id2 (account2 'id))
        (serializer1 (account1 'serializer))
        (serializer2 (account2 'serializer)))
    (cond ((< id1 id2) ((serializer1 (serializer2 exchange)) account1 account2))
          ((< id2 id1) ((serializer2 (serializer1 exchange)) account1 account2)))))

; A calls C which transfers A to B, B calls D which transfers B to A
; If A and B remain locked during the transfers, they cannot take place because A and B are deadlocked