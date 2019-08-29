#lang sicp

(define (square x) (* x x))

;(define (square-tree tree)
;  (cond ((null? tree) nil)
;        ((not (pair? tree)) (square tree))
;        (else (cons (square-tree (car tree))
;                    (square-tree (cdr tree))))))

(define (map proc items)
  (if (null? items)
      nil
      (cons (proc (car items))
            (map proc (cdr items)))))

;(define (square-tree tree)
;  (map (lambda (subtree)
;         (if (pair? subtree)
;             (square-tree subtree)
;             (square subtree)))
;       tree))

(define (tree-map proc tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (proc tree))
        (else (cons (tree-map proc (car tree))
                    (tree-map proc (cdr tree))))))

(define (square-tree tree) (tree-map square tree))

;(square-tree
; (list 1
;       (list 2 (list 3 4) 5)
;       (list 6 7)))

; Start with empty set
; For each additional element append to the previous subsets the subsets that we get from adding the element to the previous subsets
; All subsets not containing car + all subsets containing car
(define (subsets s)
  (if (null? s)
      (list nil)
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (set)
                            (append (list (car s)) set))
                          rest)))))

(subsets (list 1 2 3))