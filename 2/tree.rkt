#lang sicp

;(list 1 (list 2 (list 3 4)))

;(car (cdr (car (cdr (cdr (list 1 3 (list 5 7) 9))))))

;(car (car (list (list 7))))

;(car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7))))))))))))))))))

;(define x (list 1 2 3))
;(define y (list 4 5 6))
;(append x y) ; (1 2 3 4 5 6)
;(cons x y) ; ((1 2 3) 4 5 6)
;(list x y) ; ((1 2 3) (4 5 6))

(define (reverse items)
  (define (iter items result)
    (if (null? items)
        result
        (iter (cdr items) (cons (car items) result))))
  (iter items nil))

(define (deep-reverse items)
  (define (iter items result)
    (cond ((null? items) result)
          ((pair? (car items)) (iter (cdr items) (cons (deep-reverse (car items)) result)))
          (else (iter (cdr items) (cons (car items) result)))))
  (iter items nil))

(define x (list (list 1 2) (list 3 4)))
;(reverse x)
;(deep-reverse x)

(define (fringe items)
  (cond ((null? items) nil)
        ((pair? items) (append (fringe (car items)) (fringe (cdr items))))
        (else (list items))))

;(fringe x)
;(fringe (list x x))

(define (make-mobile left right) (list left right))
(define (make-branch length structure) (list length structure))
(define (left-branch mobile) (car mobile))
(define (right-branch mobile) (cadr mobile))
(define (branch-length branch) (car branch))
(define (branch-structure branch) (cadr branch))
(define (total-weight mobile)
  (cond ((null? mobile) 0)
        ((not (pair? mobile)) mobile)
        (else (+ (total-weight (branch-structure (left-branch mobile))) (total-weight (branch-structure (right-branch mobile)))))))

(define z (make-mobile (make-branch 5 (make-mobile (make-branch 5 3) (make-branch 5 3))) (make-branch 5 6)))
;(total-weight z)

(define (balanced? mobile)
  (define (balanced mobile)
    (= (* (branch-length (left-branch mobile)) (total-weight (branch-structure (left-branch mobile))))
       (* (branch-length (right-branch mobile)) (total-weight (branch-structure (right-branch mobile))))))
  (if (or (null? mobile) (not (pair? mobile)))
      true
      (and (balanced mobile)
           (balanced? (branch-structure (left-branch mobile)))
           (balanced? (branch-structure (right-branch mobile))))))

(balanced? z)