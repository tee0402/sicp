#lang sicp

(define (make-from-mag-ang r a)
  (define (dispatch op)
    (cond ((eq? op 'real-part) (* r (cos a)))
          ((eq? op 'imag-part) (* r (sin a)))
          ((eq? op 'magnitude) r)
          ((eq? op 'angle) a)
          (else (error "Unknown op: MAKE-FROM-MAG-ANG" op))))
  dispatch)

; Explicit dispatch on type:
; To add new types, create a new set of procedures with non-conflicting names and add a new type
; check in each existing generic procedure
; To add new operations, create a new procedure with non-conflicting names for each existing type
; and add a new generic procedure

; Data-directed programming:
; To add new types, create a new set of procedures and put each definition into a table
; To add new operations, create a new procedure for each existing type and put each definition into a table

; Message passing:
; To add new types, create a procedure that returns a procedure which takes an operation and performs it
; To add new operations, create a new operation check in each existing type procedure

; If new types must often be added, message passing would be most appropriate because the additions are
; self-contained and additive: you do not have to worry about name conflicts or modifying existing
; procedures, and it is more concise than data-directed programming
; If new operations must often be added, data-directed programming would be most appropriate because you
; do not have to worry about name conflicts or modifying existing procedures, you just have to modify
; each installation to add the operation