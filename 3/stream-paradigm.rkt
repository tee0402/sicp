#lang sicp

; By defining guesses, we retain the pointer to the stream which memoizes the next guess when it is computed, which points to the next
; memoization, and so on. So in essence, we are storing the stream as it is being generated. In contrast, calling a procedure creates a
; stream that cannot be referred to again, so we lose access to the memoizations. This means that sqrt-stream has to generate a new
; stream on every cdr even though the stream was previously generated
; No

(define (stream-limit s tolerance)
  (define (iter s prev)
    (let ((curr (stream-car s)))
      (if (< (abs (- curr prev)) tolerance)
          curr
          (iter (stream-cdr s) curr))))
  (iter (stream-cdr s) (stream-car s)))

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))
(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams))
       (apply stream-map
              (cons proc (map stream-cdr argstreams))))))
(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin (proc (stream-car s))
             (stream-for-each proc (stream-cdr s)))))
(define (display-stream s)
  (stream-for-each display-line s))
(define (display-line x) (newline) (display x))
(define (stream-car stream) (car stream))
(define (stream-cdr stream) (force (cdr stream)))
(define (partial-sums s)
  (cons-stream (stream-car s) (add-streams (stream-cdr s) (partial-sums s))))
(define (add-streams s1 s2) (stream-map + s1 s2))
(define (square x) (* x x))

; ln 2 = 0.693147180559945
(define (ln-summands n)
  (cons-stream (/ 1.0 n)
               (stream-map - (ln-summands (+ n 1)))))
(define ln-stream (partial-sums (ln-summands 1)))
;(display-stream ln-stream)
; 0.7595 - 0.6345
(define (euler-transform s)
  (let ((s0 (stream-ref s 0)) ; Sn-1
        (s1 (stream-ref s 1)) ; Sn
        (s2 (stream-ref s 2))) ; Sn+1
    (cons-stream (- s2 (/ (square (- s2 s1))
                          (+ s0 (* -2 s1) s2)))
                 (euler-transform (stream-cdr s)))))
;(display-stream (euler-transform ln-stream))
; 0.6933 - 0.6930
(define (make-tableau transform s)
  (cons-stream s (make-tableau transform (transform s))))
(define (accelerated-sequence transform s)
  (stream-map stream-car (make-tableau transform s)))
;(display-stream (accelerated-sequence euler-transform ln-stream))
; 0.6931471805599445 - 14 digits

(define ones (cons-stream 1 ones))
(define integers
  (cons-stream 1 (add-streams ones integers)))
(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream (stream-car s1)
                   (interleave s2 (stream-cdr s1)))))
(define (pairs s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave
    (stream-map (lambda (x) (list (stream-car s) x))
                (stream-cdr t))
    (pairs (stream-cdr s) (stream-cdr t)))))
;(display-stream (pairs integers integers))
; 2^n - 2 pairs precede (n n)
; Then you see the pair (n n+1) after 2^(n - 1) pairs, and you see a pair (n x) for x > n + 1 every 2^n pairs
; So:
; 2^n - 2 pairs precede (n n)
; 2^n - 2 + 2^(n - 1) pairs precede (n n+1)
; 2^n - 2 + 2^(n - 1) + (x - n - 1) * 2^n = (x - n) * 2^n - 2 + 2^(n - 1) pairs precede (n x) for x > n + 1
; So 197 pairs precede (1 100), 2^99 - 2 + 2^98 pairs precede (99 100) and 2^100 - 2 pairs precede (100 100)

;(define (pairs s t)
;  (cons-stream
;   (list (stream-car s) (stream-car t))
;   (interleave
;    (interleave
;     (stream-map (lambda (x) (list (stream-car s) x))
;                 (stream-cdr t))
;     (stream-map (lambda (x) (list x (stream-car t)))
;                 (stream-cdr s)))
;    (pairs (stream-cdr s) (stream-cdr t)))))

; Since interleave is not a special form, all of its arguments are evaluated when it is called, so pairs will go on a cdr loop

(define (stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
        ((pred (stream-car stream))
         (cons-stream (stream-car stream)
                      (stream-filter
                       pred
                       (stream-cdr stream))))
        (else (stream-filter pred (stream-cdr stream)))))
(define (triples s t u)
  (cons-stream
   (list (stream-car s) (stream-car t) (stream-car u))
   (cons-stream
    (list (stream-car s) (stream-car t) (stream-car (stream-cdr u)))
    (interleave
     (stream-map (lambda (pair) (cons (stream-car s) pair))
                 (pairs (stream-cdr t) (stream-cdr u)))
     (triples (stream-cdr s) (stream-cdr t) (stream-cdr u))))))
(define pythagorean-triples (stream-filter (lambda (triple) (= (+ (square (car triple)) (square (cadr triple))) (square (caddr triple)))) (triples integers integers integers)))
;(display-stream pythagorean-triples)

(define (merge-weighted s1 s2 weight)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
         (let ((s1car (stream-car s1))
               (s2car (stream-car s2)))
           (let ((s1weight (weight s1car))
                 (s2weight (weight s2car)))
             (if (<= s1weight s2weight)
                 (cons-stream
                  s1car
                  (merge-weighted (stream-cdr s1) s2 weight))
                 (cons-stream
                  s2car
                  (merge-weighted s1 (stream-cdr s2) weight))))))))
(define (weighted-pairs s t weight)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (merge-weighted
    (stream-map (lambda (x) (list (stream-car s) x))
                (stream-cdr t))
    (weighted-pairs (stream-cdr s) (stream-cdr t) weight)
    weight)))
(define sum-pairs (weighted-pairs integers integers (lambda (pair) (+ (car pair) (cadr pair)))))
;(display-stream sum-pairs)
(define (divisible? x y) (= (remainder x y) 0))
(define weird-pairs (stream-filter (lambda (pair) (let ((one (car pair)) (two (cadr pair))) (not (or (even? one) (even? two) (divisible? one 3) (divisible? two 3) (divisible? one 5) (divisible? two 5)))))
                                   (weighted-pairs integers integers (lambda (pair) (+ (* 2 (car pair)) (* 3 (cadr pair)) (* 5 (car pair) (cadr pair)))))))
;(display-stream weird-pairs)
(define (sum-pair proc) (lambda (pair) (+ (proc (car pair)) (proc (cadr pair)))))
(define (cube x) (* x x x))
(define ramanujan-pairs (weighted-pairs integers integers (sum-pair cube)))
;(display-stream ramanujan-pairs)
(define (ramanujan-numbers)
  (define (iter s prev-weight)
    (let ((curr-weight ((sum-pair cube) (stream-car s))))
      (if (= prev-weight curr-weight)
          (display-line prev-weight))
      (iter (stream-cdr s) curr-weight)))
  (iter ramanujan-pairs 0))
;(ramanujan-numbers)
; 1729, 4104, 13832, 20683, 32832, 39312

(define square-sum-pairs (weighted-pairs integers integers (sum-pair square)))
;(display-stream square-sum-pairs)
(define (prev-pairs weight prev) (cons weight prev))
(define (weight prev-pairs) (car prev-pairs))
(define (prev prev-pairs) (cdr prev-pairs))
(define (len prev-pairs) (length (prev prev-pairs)))
(define (square-sum-three-ways)
  (define (iter s prevs)
    (let ((prev-weight (weight prevs))
          (curr (stream-car s)))
      (let ((curr-weight ((sum-pair square) curr)))
        (if (= prev-weight curr-weight)
            (iter (stream-cdr s) (prev-pairs prev-weight (cons curr (prev prevs))))
            (begin (if (>= (len prevs) 3)
                       (display-line prevs))
                       (iter (stream-cdr s) (prev-pairs curr-weight (list curr))))))))
  (iter square-sum-pairs (prev-pairs 0 '())))
;(square-sum-three-ways)

(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor))
              stream))
(define (integral integrand initial-value dt)
  (define int
    (cons-stream initial-value
                 (add-streams (scale-stream integrand dt)
                              int)))
  int)
(define (RC R C dt)
  (lambda (i v0) (add-streams (scale-stream i R)
                              (integral (scale-stream i (/ 1.0 C)) v0 dt))))

;(define zero-crossings
;  (stream-map sign-change-detector
;              sense-data
;              (cons-stream 0 sense-data)))

(define (make-zero-crossings input-stream last-value last-smoothed)
  (let ((avpt (/ (+ (stream-car input-stream)
                    last-value)
                 2)))
    (cons-stream
     (sign-change-detector avpt last-smoothed)
     (make-zero-crossings
      (stream-cdr input-stream) (stream-car input-stream) avpt))))

(define (smooth s)
  (stream-map average s (stream-cdr s)))
(define zero-crossings
  (let ((smoothed (smooth sense-data)))
    (stream-map sign-change-detector
                smoothed
                (cons-stream 0 smoothed))))