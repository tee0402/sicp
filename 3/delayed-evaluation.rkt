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
(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor))
              stream))
(define (add-streams s1 s2) (stream-map + s1 s2))

(define (integral delayed-integrand initial-value dt)
  (cons-stream
   initial-value
   (let ((integrand (force delayed-integrand)))
     (if (stream-null? integrand)
         the-empty-stream
         (integral (delay (stream-cdr integrand))
                   (+ (* dt (stream-car integrand))
                      initial-value)
                   dt)))))

;(define (solve-2nd a b dt y0 dy0)
;  (define y (integral (delay dy) y0 dt))
;  (define dy (integral (delay ddy) dy0 dt))
;  (define ddy (add-streams (scale-stream dy a)
;                           (scale-stream y b)))
;  y)

(define (solve-2nd f dt y0 dy0)
  (define y (integral (delay dy) y0 dt))
  (define dy (integral (delay ddy) dy0 dt))
  (define ddy (stream-map f dy y))
  y)

(define (RLC R L C dt)
  (lambda (vC0 iL0)
    (define vC (integral (delay dvC) vC0 dt))
    (define iL (integral (delay diL) iL0 dt))
    (define dvC (scale-stream iL (/ -1.0 C)))
    (define diL (add-streams (scale-stream vC (/ 1.0 L))
                             (scale-stream iL (/ (- R) L))))
    
    (cons vC iL)))
;((RLC 1 1 0.2 0.1) 10 0)