#lang sicp

;(define (neg x) (apply-generic 'neg x))

; In polynomial package
(define (=zero? p)
  (cond ((empty-termlist? p) false)
        ((=zero? (coeff (first-term p))) true)
        (else (=zero? (rest-terms p)))))
(define (neg p)
  (if (empty-termlist? p)
      p
      (adjoin-term (make-term (order (first-term p)) (neg (coeff (first-term p)))) (rest-terms p))))
(put 'neg '(polynomial) (lambda (p) (tag (neg p))))

; In scheme-number package
(put 'neg '(scheme-number) (lambda (x) (tag (- x))))
; In rational package
(put 'neg '(rational) (lambda (x) (tag (make-rat (- (numer x)) (- (denom x))))))
; In complex package
(put 'neg '(complex) (lambda (x) (tag (make-from-real-imag (- (real-part x)) (- (imag-part x))))))

(define (sub-poly p1 p2)
  (if (same-variable? (variable p1) (variable p2))
      (make-poly (variable p1)
                 (sub-terms (term-list p1) (term-list p2)))
      (error "Polys not in same var: SUB-POLY" (list p1 p2))))
(define (sub-terms L1 L2)
  (cond ((empty-termlist? L1) (neg L2))
        ((empty-termlist? L2) L1)
        (else
         (let ((t1 (first-term L1))
               (t2 (first-term L2)))
           (cond ((> (order t1) (order t2))
                  (adjoin-term
                   t1 (sub-terms (rest-terms L1) L2)))
                 ((< (order t1) (order t2))
                  (adjoin-term
                   (neg t2) (sub-terms L1 (rest-terms L2))))
                 (else
                  (adjoin-term
                   (make-term (order t1)
                              (sub (coeff t1) (coeff t2)))
                   (sub-terms (rest-terms L1)
                              (rest-terms L2)))))))))

(define (adjoin-term term term-list) (apply-generic 'adjoin-term term term-list))
(define (the-empty-termlist) ((get 'the-empty-termlist 'sparse)))
(define (the-empty-termlist-sparse) ((get 'the-empty-termlist 'sparse)))
(define (the-empty-termlist-dense) ((get 'the-empty-termlist 'dense)))
(define (first-term term-list) (apply-generic 'first-term term-list))
(define (rest-terms term-list) (apply-generic 'rest-terms term-list))
(define (empty-termlist? term-list) (apply-generic 'empty-termlist? term-list))
(define (tag-term term) (attach-tag 'term term))
(define (make-term order coeff) (tag (list order coeff)))
(define (order term) (car (contents term)))
(define (coeff term) (cadr (contents term)))

(define (install-sparse)
  (define (adjoin-term term term-list)
    (if (=zero? (coeff term))
        term-list
        (cons term term-list)))
  (define (the-empty-termlist) '())
  (define (first-term term-list) (car term-list))
  (define (rest-terms term-list) (cdr term-list))
  (define (empty-termlist? term-list) (null? term-list))
  
  (define (tag term-list) (attach-tag 'sparse term-list))
  (put 'adjoin-term '(term sparse)
       (lambda (term term-list) (tag (adjoin-term term term-list))))
  (put 'the-empty-termlist 'sparse
       (lambda () (tag (the-empty-termlist))))
  (put 'first-term '(sparse)
       (lambda (term-list) (tag-term (first-term term-list))))
  (put 'rest-terms '(sparse)
       (lambda (term-list) (tag (rest-terms term-list))))
  (put 'empty-termlist? '(sparse) empty-termlist?)
  'done)

(define (install-dense)
  (define (adjoin-term term term-list)
    (cond ((=zero? (coeff term)) term-list)
          ((= (order term) (length term-list)) (cons (coeff term) term-list))
          (else (adjoin-term term (cons 0 term-list)))))
  (define (the-empty-termlist) '())
  (define (first-term term-list) (make-term (- (length term-list) 1) (car term-list)))
  (define (rest-terms term-list)
    (define (rest term-list)
      (cond ((empty-termlist? term-list) term-list)
            ((=zero? (car term-list)) (rest (cdr term-list)))
            (else term-list)))
    (rest (cdr term-list)))
  (define (empty-termlist? term-list) (null? term-list))

  (define (tag term-list) (attach-tag 'dense term-list))
  (put 'adjoin-term '(term dense)
       (lambda (term term-list) (tag (adjoin-term term term-list))))
  (put 'the-empty-termlist '(dense)
       (lambda () (tag (the-empty-termlist))))
  (put 'first-term '(dense)
       (lambda (term-list) (tag-term (first-term term-list))))
  (put 'rest-terms '(dense)
       (lambda (term-list) (tag (rest-terms term-list))))
  (put 'empty-termlist? '(dense) empty-termlist?)
  'done)

; In polynomial package
(define (mul-terms L1 L2) (apply-generic 'mul-terms L1 L2))
(define (mul-terms-sparse L1 L2)
  (define (mul-term-by-all-terms t1 L)
    (if (empty-termlist? L)
        (the-empty-termlist-sparse)
        (let ((t2 (first-term L)))
          (adjoin-term
           (make-term (+ (order t1) (order t2))
                      (mul (coeff t1) (coeff t2)))
           (mul-term-by-all-terms t1 (rest-terms L))))))
  (if (empty-termlist? L1)
      (the-empty-termlist-sparse)
      (add-terms (mul-term-by-all-terms (first-term L1) L2)
                 (mul-terms (rest-terms L1) L2))))
(define (mul-terms-dense L1 L2)
  (define (mul-term-by-all-terms t1 L)
    (if (empty-termlist? L)
        (the-empty-termlist-dense)
        (let ((t2 (first-term L)))
          (adjoin-term
           (make-term (+ (order t1) (order t2))
                      (mul (coeff t1) (coeff t2)))
           (mul-term-by-all-terms t1 (rest-terms L))))))
  (if (empty-termlist? L1)
      (the-empty-termlist-dense)
      (add-terms (mul-term-by-all-terms (first-term L1) L2)
                 (mul-terms (rest-terms L1) L2))))
(put 'mul-terms '(sparse sparse) mul-terms-sparse)
(put 'mul-terms '(sparse dense) mul-terms-sparse)
(put 'mul-terms '(dense sparse) mul-terms-sparse)
(put 'mul-terms '(dense dense) mul-terms-dense)

(define (div-poly p1 p2)
  (if (same-variable? (variable p1) (variable p2))
      (make-poly (variable p1)
                 (div-terms (term-list p1) (term-list p2)))
      (error "Polys not in same var: DIV-POLY" (list p1 p2))))
(define (div-terms L1 L2)
  (if (empty-termlist? L1)
      (list (the-empty-termlist) (the-empty-termlist))
      (let ((t1 (first-term L1))
            (t2 (first-term L2)))
        (if (> (order t2) (order t1))
            (list (the-empty-termlist) L1)
            (let ((new-c (div (coeff t1) (coeff t2)))
                  (new-o (- (order t1) (order t2))))
              (let ((rest-of-result (div-terms (sub-terms L1 (mul-terms (adjoin-term (make-term new-o new-c) (the-empty-termlist)) L2)) L2)))
                (list (adjoin-term (make-term new-o new-c) (car rest-of-result)) (cdr rest-of-result))))))))

; Expand the terms using symbolic syntax, order terms by order of dominant variable, go through coefficients to see if
; they can all be expressed as polynomial of another variable (all non-constant terms contain the variable), factor that
; variable out to create a polynomial in each coefficient, and repeat until coefficients can no longer be factored

; In rational package
;(define (make-rat n d)
;  (cons n d))
(define (add-rat x y)
  (make-rat (add (mul (numer x) (denom y))
                 (mul (numer y) (denom x)))
            (mul (denom x) (denom y))))
(define (sub-rat x y)
  (make-rat (sub (mul (numer x) (denom y))
                 (mul (numer y) (denom x)))
            (mul (denom x) (denom y))))
(define (mul-rat x y)
  (make-rat (mul (numer x) (numer y))
            (mul (denom x) (denom y))))
(define (div-rat x y)
  (make-rat (mul (numer x) (denom y))
            (mul (denom x) (numer y))))

; In polynomial package
(define (gcd-poly p1 p2)
  (if (same-variable? (variable p1) (variable p2))
      (make-poly (variable p1)
                 (gcd-terms (term-list p1) (term-list p2)))
      (error "Polys not in same var: GCD-POLY" (list p1 p2))))
;(define (gcd-terms a b)
;  (if (empty-termlist? b)
;      a
;      (gcd-terms b (remainder-terms a b))))
(define (remainder-terms L1 L2) (cadr (div-terms L1 L2)))
(put 'greatest-common-divisor '(polynomial polynomial)
     (lambda (p1 p2) (tag (gcd-poly p1 p2))))

; In scheme-number package
(put 'greatest-common-divisor '(scheme-number scheme-number)
     (lambda (x y) (tag (gcd x y))))

(define (greatest-common-divisor x y) (apply-generic 'greatest-common-divisor x y))

; In polynomial package
(define (pseudoremainder-terms L1 L2)
  (cadr (div-terms (mul L1 (expt (coeff (first-term L2)) (+ 1 (order (first-term L1)) (- (order (first-term L2)))))) L2)))
(define (gcd-terms a b)
  (if (empty-termlist? b)
      (div a (gcd-coeff a))
      (gcd-terms b (pseudoremainder-terms a b))))
(define (gcd-coeff x)
  (if (empty-termlist? (rest-terms x))
      (coeff (first-term x))
      (gcd (coeff (first-term x)) (gcd-coeff (rest-terms x)))))

(define (reduce-poly p1 p2)
  (if (same-variable? (variable p1) (variable p2))
      (make-poly (variable p1)
                 (reduce-terms (term-list p1) (term-list p2)))
      (error "Polys not in same var: REDUCE-POLY" (list p1 p2))))
(define (reduce-terms n d)
  (let ((g (gcd-terms n d)))
    (let ((i (expt (coeff (first-term g)) (+ 1 (max (order (first-term n)) (order (first-term d))) (- (order (first-term g)))))))
      (let ((n (div-terms (mul n i) g))
            (d (div-terms (mul d i) g)))
        (let ((f (gcd (gcd-coeff n) (gcd-coeff d))))
          (list (div n f) (div d f)))))))
(put 'reduce '(polynomial polynomial)
     (lambda (p1 p2) (tag (reduce-poly p1 p2))))

; In scheme-number package
(define (reduce-integers n d)
  (let ((g (gcd n d)))
    (list (/ n g) (/ d g))))
(put 'reduce '(scheme-number scheme-number)
     (lambda (x y) (tag (reduce-integers x y))))

(define (reduce n d) (apply-generic 'reduce n d))

; In rational package
(define (make-rat n d)
  (let ((reduced (reduce n d)))
    (cons (car reduced) (cadr reduced)))