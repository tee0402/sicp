#lang sicp

(define make-withdraw
  (lambda (initial-amount)
    ((lambda (balance)
       (lambda (amount)
         (if (>= balance amount)
             (begin (set! balance (- balance amount))
                    balance)
             "Insufficient funds")))
     initial-amount)))
; The two versions create objects with the same behavior since the procedure objects created across both versions have the same code and
; point to a frame containing their own balance which they can modify
; The version that uses let to create the local state variable explicitly creates an additional frame that contains initial-amount and
; is pointed to by the frame containing balance
; The additional frame exists unnecessarily because initial-amount is never accessed but the frame is still relevant because it is still
; being pointed to

; The local states for the two accounts are kept in different frames in different environments
; The code for withdraw, deposit, and dispatch are shared between acc and acc2