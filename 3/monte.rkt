#lang sicp

(define (square x) (* x x))

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1)
                 (+ trials-passed 1)))
          (else
           (iter (- trials-remaining 1)
                 trials-passed))))
  (iter trials 0))

(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))

(define (estimate-integral P x1 x2 y1 y2 trials)
  (* (monte-carlo trials (P x1 x2 y1 y2)) (area x1 x2 y1 y2)))

(define (area x1 x2 y1 y2)
  (* (- x2 x1) (- y2 y1)))

(define (unit-circle x1 x2 y1 y2)
  (lambda () (<= (+ (square (random-in-range x1 x2)) (square (random-in-range y1 y2))) 1)))

(define (estimate-pi trials)
  (estimate-integral unit-circle -1.0 1.0 -1.0 1.0 trials))

;(estimate-pi 100000)

(define random-init 0)
(define rand
  (let ((x random-init))
    (lambda (m)
      (cond ((eq? m 'generate) (set! x (rand-update x)) x)
            ((eq? m 'reset) (lambda (init) (set! x init)))
            (else (error "Unknown request: RAND" m))))))