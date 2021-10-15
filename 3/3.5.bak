#lang sicp

(define (stream-car stream) (car stream))
(define (stream-cdr stream) (force (cdr stream)))
(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams))
       (apply stream-map
              (cons proc (map stream-cdr argstreams))))))

(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream
       low
       (stream-enumerate-interval (+ low 1) high))))
;(define (stream-map proc s)
;  (if (stream-null? s)
;      the-empty-stream
;      (cons-stream (proc (stream-car s))
;                   (stream-map proc (stream-cdr s)))))
(define (show x)
  (display-line x)
  x)
(define (display-line x) (newline) (display x))
(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))
; (define x
;   (stream-map show
;               (stream-enumerate-interval 0 10)))
; 0
; x = (cons 0 (delay (stream-map show (stream-cdr (cons 0 (delay (stream-enumerate-interval 1 10)))))))
; (stream-ref x 5)
; 1
; 2
; 3
; 4
; 55
; Since delay is memoized, stream-map and stream-enumerate-interval results from 1 to 5 are saved as they are computed
; Our definition of x in the beginning allows us access to the memoizations through the delayed object in x, which contains the first stream-map
; result which contains the delayed object containing the second stream-map result, and so on
; The second time we stream-cdr through x, we just use the memoizations, so stream-map is not computed for 1 to 5 and they are not printed
; (stream-ref x 7)
; 6
; 77

(define (stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
        ((pred (stream-car stream))
         (cons-stream (stream-car stream)
                      (stream-filter
                       pred
                       (stream-cdr stream))))
        (else (stream-filter pred (stream-cdr stream)))))
(define (display-stream s)
  (stream-for-each display-line s))
(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin (proc (stream-car s))
             (stream-for-each proc (stream-cdr s)))))
; (define sum 0)
; sum = 0
; (define (accum x) (set! sum (+ x sum)) sum)
; sum = 0
; Defines the sequence of the 1st to the 20th triangular number
; (define seq
;   (stream-map accum
;               (stream-enumerate-interval 1 20)))
; sum = 1
; seq = (cons 1 (delay (stream-map accum (stream-cdr (cons 1 (delay (stream-enumerate-interval 2 20)))))))
; (define y (stream-filter even? seq))
; sum = 6
; y = (cons 6 (delay (stream-filter even? (stream-cdr (cons 6 (delay (stream-map accum (stream-cdr (cons 3 (delay (stream-enumerate-interval 4 20)))))))))))
; Since delay is memoized, stream-map will not be called again for 2 to 3 and they will not be added to sum again
; (define z
;   (stream-filter (lambda (x) (= (remainder x 5) 0))
;                  seq))
; sum = 10
; z = (cons 10 (delay (stream-filter (lambda (x) (= (remainder x 5) 0)) (stream-cdr (cons 10 (delay (stream-map accum (stream-cdr (cons 4 (delay (stream-enumerate-interval 5 20)))))))))))
; Since delay is memoized, stream-map will not be called again for 4 and it will not be added to sum again
; (stream-ref y 7)
; sum = 136
; 136
; Since delay is memoized, stream-map will not be called again for 5 to 16 and they will not be added to sum again
; When the interpreter prints an evaluated value, it prints the value and then adds a new line
; When display-line prints a value, it adds a new line and then prints the value
; (display-stream z)
; sum = 210
; 
; 10
; 15
; 45
; 55
; 105
; 120
; 190
; 210done
; The responses would differ if we did not memoize delay because (define z ...) would accumulate 2 to 3 again, (stream-ref y 7) would
; accumulate 4 again, and (display-stream z) would accumulate 5 to 16 again

; s produces the stream of powers of 2

(define ones (cons-stream 1 ones))
(define (add-streams s1 s2) (stream-map + s1 s2))
(define integers
  (cons-stream 1 (add-streams ones integers)))
(define (mul-streams s1 s2) (stream-map * s1 s2))
(define factorials
  (cons-stream 1 (mul-streams factorials (stream-cdr integers))))

(define (partial-sums s)
  (cons-stream (stream-car s) (add-streams (stream-cdr s) (partial-sums s))))

(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor))
              stream))
(define (merge s1 s2)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
         (let ((s1car (stream-car s1))
               (s2car (stream-car s2)))
           (cond ((< s1car s2car)
                  (cons-stream
                   s1car
                   (merge (stream-cdr s1) s2)))
                 ((> s1car s2car)
                  (cons-stream
                   s2car
                   (merge s1 (stream-cdr s2))))
                 (else
                  (cons-stream
                   s1car
                   (merge (stream-cdr s1)
                          (stream-cdr s2)))))))))
(define S (cons-stream 1 (merge (scale-stream S 2) (merge (scale-stream S 3) (scale-stream S 5)))))

; n - 1 additions are performed when we compute the nth Fibonacci number using the add-streams definition, since we perform an addition to
; compute each additional Fibonacci number after the 1st
; Without memoization, the number of additions performed would be exponentially greater because a tree-recursive process would be generated
; from the need to recompute Fibonacci numbers that have already been computed

; It is the fractional part of the base radix result of the long division of num divided by den
; (expand 1 7 10) = (1 4 2 8 5 7 ...)
; (expand 3 8 10) = (3 7 5 0 0 0 ...)

(define (integrate-series s) (stream-map / s integers))
; Maclaurin series for cosine and sine
(define cosine-series (cons-stream 1 (integrate-series (scale-stream sine-series -1))))
(define sine-series (cons-stream 0 (integrate-series cosine-series)))

(define (mul-series s1 s2)
  (cons-stream (* (stream-car s1) (stream-car s2)) (add-streams (add-streams (scale-stream (stream-cdr s1) (stream-car s2))
                                                                             (scale-stream (stream-cdr s2) (stream-car s1)))
                                                                (cons-stream 0 (mul-series (stream-cdr s1) (stream-cdr s2))))))
; sin^2 + cos^2
; (display-stream (add-streams (mul-series sine-series sine-series) (mul-series cosine-series cosine-series)))

(define (invert-unit-series s)
  (cons-stream 1 (scale-stream (mul-series (stream-cdr s) (invert-unit-series s)) -1)))

(define (div-series s1 s2)
  (if (= (stream-car s2) 0)
      (error "Denominator has a zero constant term")
      (mul-series s1 (scale-stream (invert-unit-series s2) (/ 1 (stream-car s2))))))
(define tangent-series (div-series sine-series cosine-series))
;(display-stream tangent-series)