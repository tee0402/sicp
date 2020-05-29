#lang sicp

; magnitude does not really act like a generic procedure at the top level because it only works on complex numbers
; It simply strips off the complex tag and calls itself again, at which point it directs the data to the right representation package
; apply-generic is called twice: magnitude calls apply-generic, which strips off the complex tag and calls magnitude again, which calls
; apply-generic, which strips off the rectangular tag and calls the magnitude procedure for the rectangular representation, returning 5

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

;(define (apply-generic op . args)
;  (let ((type-tags (map type-tag args)))
;    (let ((proc (get op type-tags)))
;      (if proc
;          (apply proc (map contents args))
;          (error
;           "No method for these types: APPLY-GENERIC"
;           (list op type-tags))))))
;(define (equ? x y) (apply-generic 'equ? x y))
; In install-scheme-number-package:
;(define (equ? x y) (= x y))
;(put 'equ? '(scheme-number scheme-number) equ?)
; In install-rational-package:
;(define (equ? x y) (and (= (numer x) (numer y)) (= (denom x) (denom y))))
;(put 'equ? '(rational rational) equ?)
; In install-complex-package:
;(define (equ? x y) (and (= (real-part x) (real-part y)) (= (imag-part x) (imag-part y))))
;(put 'equ? '(complex complex) equ?)

;(define (=zero? x) (apply-generic '=zero? x))
; In install-scheme-number-package:
;(define (=zero? x) (= x 0))
;(put '=zero? '(scheme-number) =zero?)
; In install-rational-package:
;(define (=zero? x) (= (numer x) 0))
;(put '=zero? '(rational) =zero?)
; In install-complex-package:
;(define (=zero? x) (= (magnitude x) 0))
;(put '=zero? '(complex) =zero?)

; apply-generic will go into an infinite loop, since it will keep calling itself with a coerced but unchanged first argument
; Louis is correct that apply-generic will unnecessarily try to coerce arguments of the same type if there is no defined procedure,
; which also happens after it coerces arguments of different types into each other
; (if (and (= (length args) 2) (not (eq? (car type-tags) (cadr type-tags))))

(define (all-equal? items)
  (let ((rest (cdr items)))
    (if (null? rest)
      true
      (and (equal? (car items) (car rest)) (all-equal? rest)))))
(define (all-true? items)
  (cond ((null? items) true)
        ((car items) (all-true? (cdr items)))
        (else false)))
(define (apply-ops ops args)
  (if (null? ops)
      '()
      (cons ((car ops) (car args)) (apply-ops (cdr ops) (cdr args)))))
;(define (apply-generic op . args)
;  (let ((type-tags (map type-tag args)))
;    (define (coercion-iter curr-args)
;      (if (null? curr-args)
;          (error "No method for these types"
;                 (list op type-tags))
;          (let ((try-type (type-tag (car curr-args))))
;            (let ((coercion-procs (map (lambda (type)
;                                         (if (eq? type try-type)
;                                             (lambda (x) x)
;                                             (get-coercion type try-type)))
;                                       type-tags)))
;              (if (all-true? coercion-procs)
;                  (let ((new-args (apply-ops coercion-procs args)))
;                    (let ((proc (get op (map type-tag new-args))))
;                      (if proc
;                          (apply proc (map contents new-args))
;                          (coercion-iter (cdr curr-args)))))
;                  (coercion-iter (cdr curr-args)))))))
;    (let ((proc (get op type-tags)))
;      (if proc
;          (apply proc (map contents args))
;          (if (not (all-equal? type-tags))
;              (coercion-iter args)
;              (error "No method for these types"
;                     (list op type-tags)))))))
; This strategy is not general enough if, for example:
; 1. Types 1 and 2 can be coerced into type 3 and combined, but it will not be tried
; 2. Types 1 and 2 can be coerced into each other, but only the operation of (type 2, type 1) is defined, but it will not be tried

;(define (integer->rational n)
;  (make-rational (contents n) 1))
;(put 'raise 'integer integer->rational)
; In install-rational-package:
;(define (rational->real n)
;  (let ((rat (contents n)))
;    (make-real (/ (numer rat) (denom rat)))))
;(put 'raise 'rational rational->real)
;(define (real->complex n)
;  (make-complex-from-real-imag (contents n) 0))
;(put 'raise 'real real->complex)
;(define (raise arg) (apply-generic 'raise arg))

;(define (successive-raise arg)
;  (let ((raise-op (get 'raise (type-tag arg))))
;    (if raise-op
;        (successive-raise (raise-op arg))
;        arg)))
;(define (apply-generic op . args)
;  (let ((type-tags (map type-tag args)))
;    (let ((proc (get op type-tags)))
;      (if proc
;          (apply proc (map contents args))
;          (if (not (all-equal? type-tags))
;              (let ((new-args (map successive-raise args)))
;                (let ((new-type-tags (map type-tag new-args)))
;                  (if (not (equal? type-tags new-type-tags))
;                      (let ((proc (get op new-type-tags)))
;                        (if proc
;                            (apply proc (map contents new-args))
;                            (error "No method for these types"
;                                   (list op type-tags))))
;                      (error "No method for these types"
;                             (list op type-tags)))))
;              (error "No method for these types"
;                     (list op type-tags)))))))

; In install-rational-package:
;(define (rational->integer n)
;  (let ((rat (contents n)))
;    (make-scheme-number (quotient (numer rat) (denom rat)))))
;(put 'project 'rational rational->integer)
;(define (real->rational n)
;  (make-rational (round (contents n)) 1))
;(put 'project 'real real->rational)
; In install-complex-package:
;(define (complex->real n)
;  (make-real (real-part (contents n))))
;(put 'project 'complex complex->real)
;(define (project arg) (apply-generic 'project arg))
;(define (drop arg)
;  (let ((project-op (get 'project (type-tag arg))))
;    (if project-op
;        (let ((projected-arg (project-op arg)))
;          (let ((raise-op (get 'raise (type-tag projected-arg))))
;            (if raise-op
;                (if (equ? projected-arg (raise-op projected-arg))
;                    (drop projected-arg)
;                    arg)
;                arg)))
;        arg)))
;(define (apply-generic op . args)
;  (let ((type-tags (map type-tag args)))
;    (let ((proc (get op type-tags)))
;      (if proc
;          (drop (apply proc (map contents args)))
;          (if (not (all-equal? type-tags))
;              (let ((new-args (map successive-raise args)))
;                (let ((new-type-tags (map type-tag new-args)))
;                  (if (not (equal? type-tags new-type-tags))
;                      (let ((proc (get op new-type-tags)))
;                        (if proc
;                            (drop (apply proc (map contents new-args)))
;                            (error "No method for these types"
;                                   (list op type-tags))))
;                      (error "No method for these types"
;                             (list op type-tags)))))
;              (error "No method for these types"
;                     (list op type-tags)))))))

; We will assume that make-complex-from-real-imag and make-complex-from-mag-ang can take any type of number
; Therefore, the operators, constructors, and selectors of complex numbers must use generic procedures to operate on their parts
; We must install these generic procedures in the various types of numbers that a complex number can use
; In install-scheme-number-package:
;(put 'cosine '(scheme-number)
;     (lambda (x) (tag (cos x))))
;(put 'sine '(scheme-number)
;     (lambda (x) (tag (sin x))))
;(put 'square '(scheme-number)
;     (lambda (x) (tag (square x))))
;(put 'sqrt '(scheme-number)
;     (lambda (x) (tag (sqrt x))))
;(put 'atan '(scheme-number)
;     (lambda (x) (tag (atan x))))
; In install-rational-package:
;(define (cosine x)
;  (make-rat (cos (/ (numer x) (denom x))) 1))
;(put 'cosine '(rational) cosine)
;(define (sine x)
;  (make-rat (sin (/ (numer x) (denom x))) 1))
;(put 'sine '(rational) sine)
;(define (square x)
;  (make-rat (square (numer x)) (square (denom x))))
;(put 'square '(rational) square)
;(define (sqrt x)
;  (make-rat (sqrt (numer x)) (sqrt (denom x))))
;(put 'sqrt '(rational) sqrt)
;(define (atan x)
;  (make-rat (atan (/ (numer x) (denom x))) 1))
;(put 'atan '(rational) atan)
;(define (cosine x) (apply-generic 'cosine x))
;(define (sine x) (apply-generic 'sine x))
;(define (square x) (apply-generic 'square x))
;(define (sqrt x) (apply-generic 'sqrt x))
;(define (atan x) (apply-generic 'atan x))
; In install-rectangular-package:
;(define (magnitude z)
;  (sqrt (add (square (real-part z))
;             (square (imag-part z)))))
;(define (angle z)
;  (atan (imag-part z) (real-part z)))
;(define (make-from-mag-ang r a)
;  (cons (mul r (cosine a)) (mul r (sine a))))
; In install-polar-package:
;(define (real-part z) (mul (magnitude z) (cosine (angle z))))
;(define (imag-part z) (mul (magnitude z) (sine (angle z))))
;(define (make-from-real-imag x y)
;  (cons (sqrt (add (square x) (square y)))
;        (atan y x)))
; In install-complex-package:
; We do not define cosine, sine, square, sqrt, and atan for complex numbers because real parts, imaginary parts, magnitudes, and angles
; cannot be complex numbers, since the operations add, sub, mul, and div are defined recursively
;(define (add-complex z1 z2)
;  (make-from-real-imag (add (real-part z1) (real-part z2))
;                       (add (imag-part z1) (imag-part z2))))
;(define (sub-complex z1 z2)
;  (make-from-real-imag (sub (real-part z1) (real-part z2))
;                       (sub (imag-part z1) (imag-part z2))))
;(define (mul-complex z1 z2)
;  (make-from-mag-ang (mul (magnitude z1) (magnitude z2))
;                     (add (angle z1) (angle z2))))
;(define (div-complex z1 z2)
;  (make-from-mag-ang (div (magnitude z1) (magnitude z2))
;                     (sub (angle z1) (angle z2))))

; In install-polynomial-package:
;(define (all-zero-terms? terms)
;  (if (empty-term-list? terms)
;      true
;      (and (=zero? (coeff (first-term terms))) (all-zero-terms? (rest-terms terms)))))
;(define (poly=zero? p)
;  (all-zero-terms? (term-list p)))
;(put '=zero? '(polynomial) poly=zero?)

;(define (neg x) (apply-generic 'neg x))
; In install-scheme-number-package:
;(put 'neg '(scheme-number) (lambda (x) (tag (- x))))
; In install-rational-package:
;(put 'neg '(rational) (lambda (x) (tag (make-rat (- (numer x)) (- (denom x))))))
; In install-complex-package:
;(put 'neg '(complex) (lambda (x) (tag (make-from-real-imag (- (real-part x)) (- (imag-part x))))))
; In install-polynomial-package:
;(define (neg-terms terms)
;  (if (empty-termlist? terms)
;      terms
;      (let ((first (first-term terms)))
;        (adjoin-term (make-term (order first) (neg (coeff first))) (neg-terms (rest-terms terms))))))
;(define (neg-poly p)
;  (neg-terms (term-list p)))
;(put 'neg '(polynomial) (lambda (p) (tag (neg-poly p))))
;(define (sub-poly p1 p2)
;  (if (same-variable? (variable p1) (variable p2))
;      (make-poly (variable p1)
;                 (sub-terms (term-list p1) (term-list p2)))
;      (error "Polys not in same var: SUB-POLY" (list p1 p2))))
;(define (sub-terms L1 L2)
;  (cond ((empty-termlist? L1) (neg L2))
;        ((empty-termlist? L2) L1)
;        (else
;         (let ((t1 (first-term L1))
;               (t2 (first-term L2)))
;           (cond ((> (order t1) (order t2))
;                  (adjoin-term
;                   t1 (sub-terms (rest-terms L1) L2)))
;                 ((< (order t1) (order t2))
;                  (adjoin-term
;                   (neg t2) (sub-terms L1 (rest-terms L2))))
;                 (else
;                  (adjoin-term
;                   (make-term (order t1)
;                              (sub (coeff t1) (coeff t2)))
;                   (sub-terms (rest-terms L1)
;                              (rest-terms L2)))))))))

;(define (adjoin-term term term-list)
;  (cond ((=zero? (coeff term)) term-list)
;        ((= (order term) (length term-list)) (cons (coeff term) term-list))
;        (else (adjoin-term term (cons 0 term-list)))))
;(define (first-term term-list) (make-term (length (cdr term-list)) (car term-list)))
;(define (rest-terms term-list)
;  (define (rest term-list)
;    (if (or (empty-termlist? term-list) (not (=zero? (car term-list))))
;        term-list
;        (rest (cdr term-list))))
;  (rest (cdr term-list)))

;(define (install-term)
;  (define (make-term order coeff) (list order coeff))
;  (define (order term) (car term))
;  (define (coeff term) (cadr term))
;  
;  (define (tag term) (attach-tag 'term term))
;  (put 'order '(term) order)
;  (put 'coeff '(term) coeff)
;  (put 'make 'term
;       (lambda (order coeff) (tag (make-term order coeff))))
;  'done)
;(define (make-term order coeff) ((get 'make 'term) order coeff))
;(define (order term) (apply-generic 'order term))
;(define (coeff term) (apply-generic 'coeff term))
;(define (install-sparse)
;  (define (adjoin-term term term-list)
;    (if (=zero? (coeff term))
;        term-list
;        (cons term term-list)))
;  (define (first-term term-list) (car term-list))
;  (define (rest-terms term-list) (cdr term-list))
;  
;  (define (tag term-list) (attach-tag 'sparse term-list))
;  (put 'adjoin-term '(term sparse)
;       (lambda (term term-list) (tag (adjoin-term term term-list))))
;  (put 'first-term '(sparse) first-term)
;  (put 'rest-terms '(sparse)
;       (lambda (term-list) (tag (rest-terms term-list))))
;  (put 'empty-termlist? '(sparse) empty-termlist?)
;  (put 'the-empty-termlist 'sparse
;       (lambda () (tag (the-empty-termlist))))
;  'done)
;(define (install-dense)
;  (define (adjoin-term term term-list)
;    (cond ((=zero? (coeff term)) term-list)
;          ((= (order term) (length term-list)) (cons (coeff term) term-list))
;          (else (adjoin-term term (cons 0 term-list)))))
;  (define (first-term term-list) (make-term (length (cdr term-list)) (car term-list)))
;  (define (rest-terms term-list)
;    (define (rest term-list)
;      (if (or (empty-termlist? term-list) (not (=zero? (car term-list))))
;          term-list
;          (rest (cdr term-list))))
;    (rest (cdr term-list)))
;
;  (define (tag term-list) (attach-tag 'dense term-list))
;  (put 'adjoin-term '(term dense)
;       (lambda (term term-list) (tag (adjoin-term term term-list))))
;  (put 'first-term '(dense) first-term)
;  (put 'rest-terms '(dense)
;       (lambda (term-list) (tag (rest-terms term-list))))
;  (put 'empty-termlist? '(dense) empty-termlist?)
;  (put 'the-empty-termlist 'dense
;       (lambda () (tag (the-empty-termlist))))
;  'done)
;(define (adjoin-term term term-list) (apply-generic 'adjoin-term term term-list))
;(define (the-empty-termlist-sparse) ((get 'the-empty-termlist 'sparse)))
;(define (the-empty-termlist-dense) ((get 'the-empty-termlist 'dense)))
;(define (first-term term-list) (apply-generic 'first-term term-list))
;(define (rest-terms term-list) (apply-generic 'rest-terms term-list))
;(define (empty-termlist? term-list) (apply-generic 'empty-termlist? term-list))
; In install-polynomial-package:
;(define (mul-terms L1 L2)
;  (if (empty-termlist? L1)
;      L1
;      (add-terms (mul-term-by-all-terms (first-term L1) L2)
;                 (mul-terms (rest-terms L1) L2))))
;(define (mul-term-by-all-terms t1 L)
;  (if (empty-termlist? L)
;      L
;      (let ((t2 (first-term L)))
;        (adjoin-term
;         (make-term (+ (order t1) (order t2))
;                    (mul (coeff t1) (coeff t2)))
;         (mul-term-by-all-terms t1 (rest-terms L))))))

; In install-polynomial-package:
;(define (div-poly p1 p2)
;  (if (same-variable? (variable p1) (variable p2))
;      (make-poly (variable p1)
;                 (div-terms (term-list p1) (term-list p2)))
;      (error "Polys not in same var: DIV-POLY" (list p1 p2))))
;(define (div-terms L1 L2)
;  (if (empty-termlist? L1)
;      (list (the-empty-termlist) (the-empty-termlist))
;      (let ((t1 (first-term L1))
;            (t2 (first-term L2)))
;        (if (> (order t2) (order t1))
;            (list (the-empty-termlist) L1)
;            (let ((new-c (div (coeff t1) (coeff t2)))
;                  (new-o (- (order t1) (order t2))))
;              (let ((rest-of-result (div-terms (sub-terms L1 (mul-term-by-all-terms (make-term new-o new-c) L2)) L2)))
;                (list (adjoin-term (make-term new-o new-c) (car rest-of-result)) (cadr rest-of-result))))))))

; make-poly must check and rearrange the term list before constructing the poly:
; 1. Expand the terms fully
; 2. Identify the most dominant variable
; 3. Factor it out
; 4. Order the terms by their order
; 5. Repeat steps 2 and 3 for the coefficients until there is only one variable left

; In install-rational-package:
;(define (make-rat n d) (cons n d))
;(define (add-rat x y)
;  (make-rat (add (mul (numer x) (denom y))
;                 (mul (numer y) (denom x)))
;            (mul (denom x) (denom y))))
;(define (sub-rat x y)
;  (make-rat (sub (mul (numer x) (denom y))
;                 (mul (numer y) (denom x)))
;            (mul (denom x) (denom y))))
;(define (mul-rat x y)
;  (make-rat (mul (numer x) (numer y))
;            (mul (denom x) (denom y))))
;(define (div-rat x y)
;  (make-rat (mul (numer x) (denom y))
;            (mul (denom x) (numer y))))

; In install-polynomial-package:
;(define (remainder-terms L1 L2) (cadr (div-terms L1 L2)))
;(define (gcd-terms a b)
;  (if (empty-termlist? b)
;      a
;      (gcd-terms b (remainder-terms a b))))
;(define (gcd-poly p1 p2)
;  (if (same-variable? (variable p1) (variable p2))
;      (make-poly (variable p1)
;                 (gcd-terms (term-list p1) (term-list p2)))
;      (error "Polys not in same var: GCD-POLY" (list p1 p2))))
;(put 'greatest-common-divisor '(polynomial polynomial)
;     (lambda (p1 p2) (tag (gcd-poly p1 p2))))
; In install-scheme-number-package:
;(put 'greatest-common-divisor '(scheme-number scheme-number)
;     (lambda (x y) (tag (gcd x y))))
;(define (greatest-common-divisor x y) (apply-generic 'greatest-common-divisor x y))

; Q1 = (x^2 - 2x + 1)(11x^2 + 7) = 11x^4 - 22x^3 + 18x^2 - 14x + 7
; Q2 = (x^2 - 2x + 1)(13x + 5) = 13x^3 - 21x^2 + 3x + 5
; (gcd-terms Q1 Q2) -> (remainder-terms Q1 Q2):
; Quotient = (11/13)x - 55/169
; Remainder (R1) = (1458/169)x^2 - (2916/169)x + 1458/169
; (gcd-terms Q2 R1) -> (remainder-terms Q2 R1):
; Quotient = (2197/1458)x + 845/1458
; Remainder = 0
; (gcd-terms R1 0)
; gcd = R1

; In install-polynomial-package:
;(define (pseudoremainder-terms L1 L2)
;  (cadr (div-terms (mul L1 (expt (coeff (first-term L2)) (+ 1 (order (first-term L1)) (- (order (first-term L2)))))) L2)))
;(define (gcd-terms a b)
;  (if (empty-termlist? b)
;      a
;      (gcd-terms b (pseudoremainder-terms a b))))
;(define (gcd-coeffs terms)
;  (if (empty-termlist? terms)
;      0
;      (gcd (coeff (first-term terms)) (gcd-coeffs (rest-term terms)))))
;(define (gcd-terms a b)
;  (if (empty-termlist? b)
;      (div a (gcd-coeffs a))
;      (gcd-terms b (pseudoremainder-terms a b))))

; In install-polynomial-package:
;(define (quotient-terms L1 L2) (car (div-terms L1 L2)))
;(define (reduce-terms n d)
;  (let ((gcd-n-d (gcd-terms n d)))
;    (let ((first-gcd-term (first-term gcd-n-d)))
;      (let ((int-factor (expt (coeff first-gcd-term) (+ 1 (max (order (first-term n)) (order (first-term d))) (- (order first-gcd-term))))))
;        (let ((nn (quotient-terms (mul n int-factor) gcd-n-d))
;              (dd (quotient-terms (mul d int-factor) gcd-n-d)))
;          (let ((gcd-nn-dd (gcd (gcd-coeffs nn) (gcd-coeffs dd))))
;            (list (div nn gcd-nn-dd) (div dd gcd-nn-dd))))))))
;(define (reduce-poly p1 p2)
;  (if (same-variable? (variable p1) (variable p2))
;      (make-poly (variable p1)
;                 (reduce-terms (term-list p1) (term-list p2)))
;      (error "Polys not in same var: REDUCE-POLY" (list p1 p2))))

;(define (reduce n d) (apply-generic 'reduce n d))
; In install-scheme-number-package:
;(define (reduce-integers n d)
;  (let ((g (gcd n d)))
;    (list (/ n g) (/ d g))))
;(put 'reduce '(scheme-number scheme-number) reduce-integers)
; In install-polynomial-package:
;(put 'reduce '(polynomial polynomial) reduce-poly)
; In install-rational-package:
;(define (make-rat n d)
;  (let ((reduced (reduce n d)))
;    (cons (car reduced) (cadr reduced))))