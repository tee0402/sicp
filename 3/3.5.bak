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

; When stream-map uses the local variable guesses, it refers to guesses for the previous guess and based on that produces a new guess
; which is memoized in guesses, which stream-map can refer back to in order to produce the next guess, so it does not have to compute the
; new guess again
; In contrast, when stream-map uses (sqrt-stream x),  it refers to the resulting stream of a new call of (sqrt-stream x) for the previous
; guess and based on that produces a new guess which is memoized in the resulting stream of the current call of (sqrt-stream x), which
; stream-map cannot refer back to in order to produce the next guess
; In addition, since stream-map must create a new call of (sqrt-stream x) for getting elements of the stream beyond the first, it results
; in a chain of (sqrt-stream x), each of which has computed one more element than the next, from the original call with all the elements
; all the way to the newest call with one element
; This means that to produce a new guess, this version must also compute all previous guesses
; If delay is not memoized, when stream-map refers back to guesses it would always find the original unchanged guesses, similar to the
; second scenario, so the two versions would not differ in efficiency

(define (stream-limit s tolerance)
  (define (iter prev s)
    (let ((curr (stream-car s)))
      (if (< (abs (- curr prev)) tolerance)
          curr
          (iter curr (stream-cdr s)))))
  (iter (stream-car s) (stream-cdr s)))

(define (square x) (* x x))
(define (euler-transform s)
  (let ((s0 (stream-ref s 0)) ; Sn-1
        (s1 (stream-ref s 1)) ; Sn
        (s2 (stream-ref s 2))) ; Sn+1
    (cons-stream (- s2 (/ (square (- s2 s1))
                          (+ s0 (* -2 s1) s2)))
                 (euler-transform (stream-cdr s)))))
(define (make-tableau transform s)
  (cons-stream s (make-tableau transform (transform s))))
(define (accelerated-sequence transform s)
  (stream-map stream-car (make-tableau transform s)))
; ln 2 = 0.693147180559945
(define (ln-summands n)
  (cons-stream (/ 1.0 n)
               (stream-map - (ln-summands (+ n 1)))))
(define ln-stream (partial-sums (ln-summands 1)))
;(display-stream ln-stream)
; 1.0
; 0.5
; 0.8333333333333333
; 0.5833333333333333
; 0.7833333333333332
; 0.6166666666666666
; 0.7595238095238095
; 0.6345238095238095
; ...
;(display-stream (euler-transform ln-stream))
; 0.7
; 0.6904761904761905
; 0.6944444444444444
; 0.6924242424242424
; 0.6935897435897436
; 0.6928571428571428
; 0.6933473389355742
; 0.6930033416875522
; ...
;(display-stream (accelerated-sequence euler-transform ln-stream))
; 1.0
; 0.7
; 0.6932773109243697
; 0.6931488693329254
; 0.6931471960735491
; 0.6931471806635636
; 0.6931471805604039
; 0.6931471805599445
; ...
; Taking 8 terms of the sequence yields the correct value of ln 2 to 14 decimal places

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
; 2^(n-1) additional pairs precede (n n+1) when including (n n)
; 2^n additional pairs precede each pair (n x) for x > n+1 when including (n x-1)
; In summary:
; 2^n - 2 pairs precede (n n)
; 2^n - 2 + 2^(n-1) pairs precede (n n+1)
; 2^n - 2 + 2^(n-1) + (x-n-1)*2^n = (x-n)*2^n - 2 + 2^(n-1) pairs precede (n x) for x > n+1
; (100-1)*2^1 - 2 + 2^(1-1) = 99*2 - 2 + 2^0 = 198 - 2 + 1 = 197 pairs precede (1 100)
; 2^99 - 2 + 2^98 pairs precede (99 100)
; 2^100 - 2 pairs precede (100 100)

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

; Unlike cons-stream, interleave is not a special form, so all of its arguments will be evaluated when it is called and pairs will go into
; an infinite loop

; We can get all the triples starting with the first element of the first stream by cons-ing it to all the pairs of the second and third streams
(define (triples s t u)
  (cons-stream
   (list (stream-car s) (stream-car t) (stream-car u))
   (interleave
    (stream-cdr (stream-map (lambda (pair) (cons (stream-car s) pair))
                            (pairs t u)))
    (triples (stream-cdr s) (stream-cdr t) (stream-cdr u)))))
;(display-stream (triples integers integers integers))
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
                 (if (equal? s1car s2car)
                     (cons-stream
                      s1car
                      (merge-weighted (stream-cdr s1) (stream-cdr s2) weight))
                     (cons-stream
                      s1car
                      (merge-weighted (stream-cdr s1) s2 weight)))
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
(define pairs-ordered-by-sum (weighted-pairs integers integers (lambda (pair) (+ (car pair) (cadr pair)))))
;(display-stream pairs-ordered-by-sum)
(define (divisible? x y) (= (remainder x y) 0))
(define non-divisible-pairs (stream-filter (lambda (pair) (not (or (even? (car pair)) (even? (cadr pair)) (divisible? (car pair) 3) (divisible? (cadr pair) 3) (divisible? (car pair) 5) (divisible? (cadr pair) 5))))
                                           (weighted-pairs integers integers (lambda (pair) (+ (* 2 (car pair)) (* 3 (cadr pair)) (* 5 (car pair) (cadr pair)))))))
;(display-stream non-divisible-pairs)

(define (sum-pair proc) (lambda (pair) (+ (proc (car pair)) (proc (cadr pair)))))
(define (cube x) (* x x x))
(define ramanujan-pairs (weighted-pairs integers integers (sum-pair cube)))
;(display-stream ramanujan-pairs)
(define (ramanujan-numbers)
  (define (iter s prev-weight)
    (let ((curr-weight ((sum-pair cube) (stream-car s))))
      (if (= prev-weight curr-weight)
          (cons-stream prev-weight (iter (stream-cdr s) curr-weight))
          (iter (stream-cdr s) curr-weight))))
  (iter ramanujan-pairs 0))
;(display-stream (ramanujan-numbers))
; 1729, 4104, 13832, 20683, 32832, 39312

(define pairs-ordered-by-sum-of-squares (weighted-pairs integers integers (sum-pair square)))
;(display-stream pairs-ordered-by-sum-of-squares)
(define (make-weight-pairlist weight pairlist) (cons weight pairlist))
(define (weight weight-pairlist) (car weight-pairlist))
(define (pairlist weight-pairlist) (cdr weight-pairlist))
(define (len weight-pairlist) (length (pairlist weight-pairlist)))
(define (sum-of-squares-three-ways)
  (define (iter s prev-weight-pairlist)
    (let ((prev-weight (weight prev-weight-pairlist))
          (curr-pair (stream-car s)))
      (let ((curr-weight ((sum-pair square) curr-pair)))
        (if (= prev-weight curr-weight)
            (iter (stream-cdr s) (make-weight-pairlist prev-weight (cons curr-pair (pairlist prev-weight-pairlist))))
            (if (>= (len prev-weight-pairlist) 3)
                (cons-stream prev-weight-pairlist (iter (stream-cdr s) (make-weight-pairlist curr-weight (list curr-pair))))
                (iter (stream-cdr s) (make-weight-pairlist curr-weight (list curr-pair))))))))
  (iter pairs-ordered-by-sum-of-squares (make-weight-pairlist 0 '())))
;(display-stream (sum-of-squares-three-ways))

(define (integral integrand initial-value dt)
  (define int
    (cons-stream initial-value
                 (add-streams (scale-stream integrand dt)
                              int)))
  int)
(define (RC R C dt)
  (lambda (i v0) (add-streams (scale-stream i R)
                              (integral (scale-stream i (/ 1.0 C)) v0 dt))))

(define sense-data (cons-stream 0 0))
(define (sign-change-detector new-value old-value) true)
(define zero-crossings
  (stream-map sign-change-detector
              sense-data
              (cons-stream 0 sense-data)))

; Need to average the new input stream value with the previous input stream value, not the last smoothed value
(define (make-zero-crossings input-stream last-value last-smoothed-value)
  (let ((avpt (/ (+ (stream-car input-stream)
                    last-value)
                 2)))
    (cons-stream
     (sign-change-detector avpt last-smoothed-value)
     (make-zero-crossings
      (stream-cdr input-stream) (stream-car input-stream) avpt))))

(define (average a b) (/ (+ a b) 2))
(define (smooth s)
  (stream-map average s (stream-cdr s)))
; First implementation
;(define zero-crossings
;  (make-zero-crossings (smooth sense-data) 0))
; Second implementation
;(define zero-crossings
;  (let ((smoothed (smooth sense-data)))
;    (stream-map sign-change-detector
;                smoothed
;                (cons-stream 0 smoothed))))

;(define (integral delayed-integrand initial-value dt)
;  (cons-stream
;   initial-value
;   (let ((integrand (force delayed-integrand)))
;     (if (stream-null? integrand)
;         the-empty-stream
;         (integral (delay (stream-cdr integrand))
;                   (+ (* dt (stream-car integrand))
;                      initial-value)
;                   dt)))))

; d^2y/dt^2 = a*dy/dt + b*y
; This is a specific second-order differential equation with f(dy/dt, y) = a*dy/dt + b*y
;(define (solve-2nd a b dt y0 dy0)
;  (define y (integral (delay dy) y0 dt))
;  (define dy (integral (delay ddy) dy0 dt))
;  (define ddy (add-streams (scale-stream dy a)
;                           (scale-stream y b)))
;  y)
;(define (solve-2nd a b dt y0 dy0)
;  (define y 0)
;  (define dy 0)
;  (define ddy 0)
;  (set! y (integral (delay dy) y0 dt))
;  (set! dy (integral (delay ddy) dy0 dt))
;  (set! ddy (add-streams (scale-stream dy a)
;                           (scale-stream y b)))
;  y)

; d^2y/dt^2 = f(dy/dt, y)
(define (solve-2nd f dt y0 dy0)
  (define y (integral (delay dy) y0 dt))
  (define dy (integral (delay ddy) dy0 dt))
  (define ddy (stream-map f dy y))
  y)
;(define (solve-2nd f dt y0 dy0)
;  (define y 0)
;  (define dy 0)
;  (define ddy 0)
;  (set! y (integral (delay dy) y0 dt))
;  (set! dy (integral (delay ddy) dy0 dt))
;  (set! ddy (stream-map f dy y))
;  y)

(define (RLC R L C dt)
  (lambda (vC0 iL0)
    (define vC (integral (delay dvC) vC0 dt))
    (define iL (integral (delay diL) iL0 dt))
    (define dvC (scale-stream iL (/ -1.0 C)))
    (define diL (add-streams (scale-stream vC (/ 1.0 L))
                             (scale-stream iL (/ (- R) L))))
    (cons vC iL)))
;(define (RLC R L C dt)
;  (lambda (vC0 iL0)
;    (define vC 0)
;    (define iL 0)
;    (define dvC 0)
;    (define diL 0)
;    (set! vC (integral (delay dvC) vC0 dt))
;    (set! iL (integral (delay diL) iL0 dt))
;    (set! dvC (scale-stream iL (/ -1.0 C)))
;    (set! diL (add-streams (scale-stream vC (/ 1.0 L))
;                           (scale-stream iL (/ (- R) L))))
;    (cons vC iL)))
;((RLC 1 1 0.2 0.1) 10 0)