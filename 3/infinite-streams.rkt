#lang sicp

; It is the stream of the powers of 2

(define (mul-streams s1 s2) (stream-map * s1 s2))
(define factorials
  (cons-stream 1 (mul-streams factorials (stream-cdr integers))))

(define (partial-sums s)
  (cons-stream (stream-car s) (add-streams (stream-cdr s) (partial-sums s))))

(define S (cons-stream 1 (merge (scale-stream S 2) (merge (scale-stream S 3) (scale-stream S 5)))))

;(define (stream-map proc . argstreams)
;  (if (stream-null? (car argstreams))
;      the-empty-stream
;      (cons-stream
;       (apply proc (map stream-car argstreams))
;       (apply stream-map
;              (cons proc (map stream-cdr argstreams))))))
;(define (add-streams s1 s2) (stream-map + s1 s2))
;(define fibs
;  (cons-stream
;   0
;   (cons-stream 1 (add-streams (stream-cdr fibs) fibs))))
;(cons-stream
; 0
; (cons-stream 1 (cons-stream
;                 1
;                 (apply stream-map
;                        (cons + (map stream-cdr (list (stream-cdr fibs) fibs)))))))
; n - 1 additions with memoization
; Without memoization the number of additions would be exponentially greater because a tree-recursive process would be generated:
; fib3 would need to add fib2 and fib1, but fib2 has to add fib1 and fib0 again, even though fib2 has already been calculated in order to
; reach fib3 in the stream

; It is fraction long division of num divided by den in base radix
; (1 4 2 8 5 ...)
; (3 7 5 0 0 ...)

(define (integrate-series s) (stream-map / s integers))

(define cosine-series (cons-stream 1 (integrate-series (scale-stream sine-series -1))))
(define sine-series (cons-stream 0 (integrate-series cosine-series)))

(define (mul-series s1 s2)
  (cons-stream (* (stream-car s1) (stream-car s2)) (add-streams (add-streams (scale-stream (stream-cdr s1) (stream-car s2))
                                                                             (scale-stream (stream-cdr s2) (stream-car s1)))
                                                                (cons-stream 0 (mul-series (stream-cdr s1) (stream-cdr s2))))))

(define (invert-unit-series s)
  (define inverted (cons-stream 1 (scale-stream (mul-series (stream-cdr s) inverted) -1)))
  inverted)

(define (div-series s1 s2)
  (if (= (stream-car s2) 0)
      (error "Denominator has a zero constant term")
      (mul-series s1 (scale-stream (invert-unit-series s2) (/ 1.0 (stream-car s2))))))

(define tangent-series (div-series sine-series cosine-series))