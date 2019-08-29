#lang sicp

(define (square x) (* x x))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

;(define (map p sequence)
;  (accumulate (lambda (x y) (cons (p x) y)) nil sequence))
(define (append seq1 seq2)
  (accumulate cons seq2 seq1))
(define (length sequence)
  (accumulate (lambda (x y) (+ 1 y)) 0 sequence))

;(map square (list 1 2 3 4 5))
;(append (list 1 2 3 4 5) (list 6 7 8 9 10))
;(length (list 1 2 3 4 5))

(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms) (+ this-coeff (* x higher-terms)))
              0
              coefficient-sequence))

;(horner-eval 2 (list 1 3 0 5 0 1))

(define (count-leaves t)
  (accumulate + 0 (map (lambda (x) (if (pair? x) (count-leaves x) 1)) t)))

;(count-leaves (list (list 1 2) (list 3 4)))

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate op init (accumulate cons nil (map (lambda (x) (car x)) seqs)))
            (accumulate-n op init (accumulate cons nil (map (lambda (x) (cdr x)) seqs))))))

;(accumulate-n + 0 (list (list 1 2 3) (list 4 5 6) (list 7 8 9) (list 10 11 12)))

(define (dot-product v w)
  (accumulate + 0 (map * v w)))
(define (matrix-*-vector m v)
  (map (lambda (x) (accumulate + 0 (accumulate-n * 1 (list x v)))) m))
(define (transpose mat)
  (accumulate-n cons nil mat))
(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (row) (map (lambda (col) (dot-product row col)) cols)) m)))

;(matrix-*-vector (list (list 1 2) (list 3 4)) (list 1 2))
;(transpose (list (list 1 2) (list 3 4)))
;(matrix-*-matrix (list (list 1 2) (list 3 4)) (list (list 1 2) (list 3 4)))

(define (fold-right op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (fold-right op initial (cdr sequence)))))

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

;(fold-right / 1 (list 1 2 3)) ; (/ 1 (/ 2 (/ 3 1))) = 3/2
;(fold-left / 1 (list 1 2 3)) ; (/ (/ (/ 1 1) 2) 3) = 1/6
;(fold-right list nil (list 1 2 3)) ; (list 1 (list 2 (list 3 nil))) = (1 (2 (3 ())))
;(fold-left list nil (list 1 2 3)) ; (list (list (list nil 1) 2) 3) = (((() 1) 2) 3)
; Commutative property

;(define (reverse sequence)
;  (fold-right (lambda (x y) (append y (list x))) nil sequence))
(define (reverse sequence)
  (fold-left (lambda (x y) (append (list y) x)) nil sequence))

(reverse (list 1 2 3 4 5))