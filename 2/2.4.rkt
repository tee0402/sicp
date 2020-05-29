#lang sicp

; Each type of expression (addition, multiplication, ...) is a type that has a defined deriv operation in the operation-and-type table
; We can get the appropriate operation from the table and apply it to the operands and the variable
; We can't assimilate number? and variable? into the data-directed dispatch because numbers and variables do not have tags that we can
; naturally use like the arithmetic expressions do, and even if we do add tags to them and incorporate them into the data-directed
; dispatch, we would need to create and use constructors for them, change all arithmetic operations to generic operations, and remove
; all tags after the entire expression is simplified in order to easily read it, making the expression invalid for another derivation
; and violating the idea of closure
(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))
(define (=number? exp num) (and (number? exp) (= exp num)))
;(define (deriv exp var)
;  (cond ((number? exp) 0)
;        ((variable? exp) (if (same-variable? exp var) 1 0))
;        (else ((get 'deriv (operator exp))
;               (operands exp) var))))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
;(define (install-sum-package)
;  (define (make-sum a1 a2)
;    (cond ((=number? a1 0) a2)
;          ((=number? a2 0) a1)
;          ((and (number? a1) (number? a2))
;           (+ a1 a2))
;          (else (list '+ a1 a2))))
;  (define (addend s) (car s))
;  (define (augend s) (cadr s))
;  (put 'deriv '+ (lambda (exp var)
;                   (make-sum (deriv (addend exp) var)
;                             (deriv (augend exp) var))))
;  'done)
;(define (install-product-package)
;  (define (make-sum a1 a2)
;    (cond ((=number? a1 0) a2)
;          ((=number? a2 0) a1)
;          ((and (number? a1) (number? a2))
;           (+ a1 a2))
;          (else (list '+ a1 a2))))
;  (define (make-product m1 m2)
;    (cond ((or (=number? m1 0) (=number? m2 0)) 0)
;          ((=number? m1 1) m2)
;          ((=number? m2 1) m1)
;          ((and (number? m1) (number? m2)) (* m1 m2))
;          (else (list '* m1 m2))))
;  (define (multiplier p) (car p))
;  (define (multiplicand p) (cadr p))
;  (put 'deriv '* (lambda (exp var)
;                   (make-sum (make-product
;                              (multiplier exp)
;                              (deriv (multiplicand exp) var))
;                             (make-product
;                              (deriv (multiplier exp) var)
;                              (multiplicand exp)))))
;  'done)
;(define (install-exponentiation-package)
;  (define (make-product m1 m2)
;    (cond ((or (=number? m1 0) (=number? m2 0)) 0)
;          ((=number? m1 1) m2)
;          ((=number? m2 1) m1)
;          ((and (number? m1) (number? m2)) (* m1 m2))
;          (else (list '* m1 m2))))
;  (define (make-exponentiation b e)
;    (cond ((=number? e 0) 1)
;          ((=number? e 1) b)
;          (else (list '** b e))))
;  (define (base e) (car e))
;  (define (exponent e) (cadr e))
;  (put 'deriv '** (lambda (exp var)
;                   (make-product
;                    (make-product (exponent exp)
;                                  (make-exponentiation (base exp) (- (exponent exp) 1)))
;                    (deriv (base exp) var))))
;  'done)
; We would need to swap 'deriv and the operator name ('+, '*, ...) in the put call in each installation

(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datum: TYPE-TAG" datum)))
(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Bad tagged datum: CONTENTS" datum)))
; The lookup procedure would need to be a generic procedure for different types of keys by using generic comparators
; The type of file (structure of the set) should be attached to the division's file
;(define (get-record name records)
;  ((get 'lookup (type-tag records)) name (contents records)))
; The type of record (structure of the set) should be attached to the employee's record
;(define (get-salary record)
;  ((get 'lookup (type-tag record)) 'salary (contents record)))
;(define (find-employee-record name records)
;  (if (null? records)
;      false
;      (let ((try (get-record name (car records))))
;        (if try
;            try
;            (find-employee-record name (cdr records))))))
; If the division or employee records of the new company are structured in a way different from the ways existing records are
; structured, lookup procedures for searching the new set structures must be created and put into the operation-and-type table

(define (make-from-mag-ang r a)
  (define (dispatch op)
    (cond ((eq? op 'real-part) (* r (cos a)))
          ((eq? op 'imag-part) (* r (sin a)))
          ((eq? op 'magnitude) r)
          ((eq? op 'angle) a)
          (else (error "Unknown op: MAKE-FROM-MAG-ANG" op))))
  dispatch)

; Dispatch on type:
; To add a new type, create a new set of constructors and selectors with unique names and modify each generic procedure to check
; for the new type
; To add a new operation, create a new procedure with a unique name for each type and create a new generic procedure
; Data-directed programming:
; To add a new type, create a new set of constructors and selectors and put them into the operation-and-type table
; To add a new operation, create a new procedure for each type, put them into the operation-and-type table, and create a new
; generic procedure
; Message passing:
; To add a new type, create a procedure that returns a procedure which takes an operation and performs it
; To add a new operation, modify each type to check for the new operation name and perform it
; If new types must often be added, message passing would be most appropriate because you can do it additively and you do not need
; to create alternative constructors, tag when constructing, or have to call put many times, as in data-directed programming
; If new operations must often be added, data-directed programming would be most appropriate because you can do it additively