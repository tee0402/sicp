#lang sicp

; apply-generic will go into an infinite loop
; It works correctly as is
; (if (and (= (length args) 2) (not (eq? (car type-tags) (cadr type-tags))))

(define (all items)
  (cond ((null? items) true)
        ((car items) (all (cdr items)))
        (else false)))

(define (apply-ops ops args)
  (if (null? ops) '()
      (cons ((car ops) (car args)) (apply-ops (cdr ops) (cdr args)))))

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (define (iter curr-args)
      (let ((curr (car curr-args)))
        (let ((curr-type-tag (type-tag curr)))
          (let ((coercions (map (lambda (type)
                                  (if (eq? type curr-type-tag)
                                      (lambda (x) x)
                                      (get-coercion type curr-type-tag)))
                                type-tags)))
            (if (all coercions)
                (let ((args (apply-ops coercions args)))
                  (let ((type-tags (map type-tag args)))
                    (let ((proc (get op type-tags)))
                      (if proc
                          (apply proc (map contents args))
                          (iter (cdr curr-args))))))
                (if (null? (cdr curr-args))
                    (error "No method for these types"
                           (list op type-tags))
                    (iter (cdr curr-args))))))))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (iter args)))))

; Types 1 and 2 can be coerced to type 3 and combined
; There are three arguments with types 1, 2 and 3. They can all be coerced into each other, but only
; the operation of types 1, 2, and 1 is defined

(define (integer->rational n)
  (make-rational (contents n) 1))
(put 'raise 'integer integer->rational)

; In rational package
(define (rational->real n)
  (let ((rat (contents n)))
    (make-real (/ (numer rat) (denom rat)))))
(put 'raise 'rational rational->real)

(define (real->complex n)
  (make-complex-from-real-imag (contents n) 0))
(put 'raise 'real real->complex)

(define (raise arg1 arg2)
  (let ((type1 (type-tag arg1))
        (type2 (type-tag arg2)))
    (if (eq? type1 type2)
        arg1
        (let ((t1-raise (get 'raise type1)))
          (if t1-raise
              (raise (t1-raise arg1) arg2)
              false)))))

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (and (= (length args) 2) (not (eq? (car type-tags) (cadr type-tags))))
              (let ((a1 (car args))
                    (a2 (cadr args)))
                (let ((a1-raised (raise a1 a2))
                      (a2-raised (raise a2 a1)))
                  (cond (a1-raised
                         (apply-generic op a1-raised a2))
                        (a2-raised
                         (apply-generic op a1 a2-raised))
                        (else (error "No method for these types"
                                     (list op type-tags))))))
              (error "No method for these types"
                     (list op type-tags)))))))

; In rational package
(define (rational->integer n)
  (let ((rat (contents n)))
    (make-scheme-number (quotient (numer rat) (denom rat)))))
(put 'project 'rational rational->integer)

(define (real->rational n)
  (make-rational (round (contents n)) 1))
(put 'project 'real real->rational)

(define (complex->real n)
  (make-real (real-part (contents n))))
(put 'project 'complex complex->real)

(define (drop arg)
  (let ((type (type-tag arg)))
    (let ((project (get 'project type)))
      (if project
          (let ((projected (project arg)))
            (let ((raise (get 'raise (type-tag projected))))
              (if raise
                  (let ((raised (raise projected)))
                    (if ((get equ? type) projected raised)
                        (drop projected)
                        arg))
                  arg)))
          arg))))

; (drop (apply proc (map contents args)))

(define (cosine x)
  (make-scheme-number (cos (contents x))))
(put 'cosine '(scheme-number) cosine)
(define (sine x)
  (make-scheme-number (sin (contents x))))
(put 'sine '(scheme-number) sine)
(define (square x)
  (let ((num (contents x))
        (make-scheme-number (* num num)))))
(put 'square '(scheme-number) square)
(define (sqrt x)
  (make-scheme-number (sqrt-custom (contents x))))
(put 'sqrt '(scheme-number) sqrt)
(define (atan x)
  (make-scheme-number (atan (contents x))))
(put 'atan '(scheme-number) atan)

; In rational package
(define (cosine x)
  (let ((rat (contents x)))
    (make-rat (cos (/ (numer rat) (denom rat))) 1)))
(put 'cosine '(rational) cosine)
(define (sine x)
  (let ((rat (contents x)))
    (make-rat (sin (/ (numer rat) (denom rat))) 1)))
(put 'sine '(rational) sine)
(define (square x)
  (let ((rat (contents x))
        (make-rat (* (numer rat) (numer rat))
                  (* (denom rat) (denom rat))))))
(put 'square '(rational) square)
(define (sqrt x)
  (let ((rat (contents x))
        (make-rat (sqrt-custom (numer rat))
                  (sqrt-custom (denom rat))))))
(put 'sqrt '(rational) sqrt)
(define (atan x)
  (let ((rat (contents x))
        (make-rat (atan (/ (numer rat) (denom rat))) 1))))
(put 'atan '(rational) atan)

(define (cosine x) (apply-generic 'cosine x))
(define (sine x) (apply-generic 'sine x))
(define (square x) (apply-generic 'square x))
(define (sqrt x) (apply-generic 'sqrt x))
(define (atan x) (apply-generic 'atan x))

; In rectangular package
(define (magnitude z)
  (sqrt (add (square (real-part z))
             (square (imag-part z)))))
(define (angle z)
  (atan (imag-part z) (real-part z)))
(define (make-from-mag-ang r a)
  (cons (mul r (cosine a)) (mul r (sine a))))
;(put 'real-part '(rectangular) (lambda (x) (contents (real-part x))))
;(put 'imag-part '(rectangular) (lambda (x) (contents (imag-part x))))
;(put 'magnitude '(rectangular) (lambda (x) (contents (magnitude x))))
;(put 'angle '(rectangular) (lambda (x) (contents (angle x))))

; In polar package
(define (real-part z) (mul (magnitude z) (cosine (angle z))))
(define (imag-part z) (mul (magnitude z) (sine (angle z))))
(define (make-from-real-imag x y)
  (cons (sqrt (add (square x) (square y)))
        (atan y x)))
;(put 'real-part '(polar) (lambda (x) (contents (real-part x))))
;(put 'imag-part '(polar) (lambda (x) (contents (imag-part x))))
;(put 'magnitude '(polar) (lambda (x) (contents (magnitude x))))
;(put 'angle '(polar) (lambda (x) (contents (angle x))))

; In complex package
; Real parts, imaginary parts, magnitudes, and angles cannot be complex
(define (add-complex z1 z2)
  (make-from-real-imag (add (real-part z1) (real-part z2))
                       (add (imag-part z1) (imag-part z2))))
(define (sub-complex z1 z2)
  (make-from-real-imag (sub (real-part z1) (real-part z2))
                       (sub (imag-part z1) (imag-part z2))))
(define (mul-complex z1 z2)
  (make-from-mag-ang (mul (magnitude z1) (magnitude z2))
                     (add (angle z1) (angle z2))))
(define (div-complex z1 z2)
  (make-from-mag-ang (div (magnitude z1) (magnitude z2))
                     (sub (angle z1) (angle z2))))