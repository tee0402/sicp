#lang sicp

(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams))
       (apply stream-map
              (cons proc (map stream-cdr argstreams))))))

;(define (stream-ref s n)
;  (if (= n 0)
;      (stream-car s)
;      (stream-ref (stream-cdr s) (- n 1))))
; argstreams:
;(list (cons 0
;            (delay (stream-enumerate-interval 1 10))))
; x:
;(cons 0
;      (delay (apply stream-map
;                    (cons show (map stream-cdr (list (cons 0
;                                                           (delay (stream-enumerate-interval 1 10)))))))))
; First expression:
; 0
; Second expression:
; 1
; 2
; 3
; 4
; 5
; 5
; Third expression:
; 6
; 7
; 7

;(define (stream-filter pred stream)
;  (cond ((stream-null? stream) the-empty-stream)
;        ((pred (stream-car stream))
;         (cons-stream (stream-car stream)
;                      (stream-filter
;                       pred
;                       (stream-cdr stream))))
;        (else (stream-filter pred (stream-cdr stream)))))
;(define (stream-for-each proc s)
;  (if (stream-null? s)
;      'done
;      (begin (proc (stream-car s))
;             (stream-for-each proc (stream-cdr s)))))
;(define (display-stream s)
;  (stream-for-each display-line s))
;(define (display-line x) (newline) (display x))
; seq:
;(cons 1
;      (delay (apply stream-map
;                    (cons accum (map stream-cdr (list (cons 1
;                                                            (delay (stream-enumerate-interval 2 20)))))))))
; y:
;(cons 6
;      (delay (stream-filter
;              even?
;              (stream-cdr (cons 6
;                                (delay (apply stream-map
;                                              (cons accum (map stream-cdr (list (cons 3
;                                                                                      (delay (stream-enumerate-interval 4 20)))))))))))))
; z:
;(cons 10
;      (delay (stream-filter
;              (lambda (x) (= (remainder x 5) 0))
;              (stream-cdr (cons 10
;                                (delay (apply stream-map
;                                              (cons accum (map stream-cdr (list (cons 5
;                                                                                      (delay (stream-enumerate-interval 6 20)))))))))))))
; sum = 0
; sum = 0
; sum = 1
; sum = 6
; sum = 10
; sum = 136
; response:
; 136
; sum = 210
; response:
; 10
; 15
; 45
; 55
; 105
; 120
; 190
; 210
; done
; The responses would differ if we did not memoize delay because (define z ...) would accumulate 2 and 3 again, (stream-ref y 7) would
; accumulate 4 again, and (display-stream z) would accumulate 5 to 16 again