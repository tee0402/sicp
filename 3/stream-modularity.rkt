#lang sicp

(define (rand input-stream)
  (define (update message num)
    (cond ((eq? message 'generate) (rand-update num))
          ((eq? message 'reset) random-init)
          (else (error "Unknown message: UPDATE" message))))
  (define random-numbers
    (cons-stream
     random-init
     (stream-map update (stream-cdr input-stream) random-numbers)))
  random-numbers)

(define (stream-car s) (car s))
(define (stream-cdr s) (force (cdr s)))
(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams))
       (apply stream-map
              (cons proc (map stream-cdr argstreams))))))
(define (monte-carlo experiment-stream passed failed)
  (define (next passed failed)
    (cons-stream
     (/ passed (+ passed failed))
     (monte-carlo
      (stream-cdr experiment-stream) passed failed)))
  (if (stream-car experiment-stream)
      (next (+ passed 1) failed)
      (next passed (+ failed 1))))
(define (square x) (* x x))
(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))
(define (estimate-integral P x1 x2 y1 y2)
  (let ((a (area x1 x2 y1 y2)))
    (stream-map (lambda (p) (* p a)) (monte-carlo (P x1 x2 y1 y2) 0 0))))
(define (area x1 x2 y1 y2)
  (* (- x2 x1) (- y2 y1)))
(define (unit-circle-stream x1 x2 y1 y2)
  (cons-stream
   (<= (+ (square (random-in-range x1 x2)) (square (random-in-range y1 y2))) 1)
   (unit-circle-stream x1 x2 y1 y2)))
(define (estimate-pi trials)
  (define (iter stream trials)
    (if (= trials 1)
        (stream-car stream)
        (iter (stream-cdr stream) (- trials 1))))
  (iter (estimate-integral unit-circle-stream -1.0 1.0 -1.0 1.0) trials))
(estimate-pi 1000000)