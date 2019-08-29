#lang sicp

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

;(define (union-set set1 set2)
;  (cond ((null? set1) set2)
;        ((null? set2) set1)
;        ((element-of-set? (car set1) set2)
;         (union-set (cdr set1) set2))
;        (else (cons (car set1) (union-set (cdr set1) set2)))))

;(define (adjoin-set x set)
;  (cons x set))

;(define (union-set set1 set2)
;  (define (union set1 set2 set3)
;    (cond ((not (null? set1))
;           (if (element-of-set? (car set1) set3)
;               (union (cdr set1) set2 set3)
;               (union (cdr set1) set2 (cons (car set1) set3))))
;          ((not (null? set2))
;           (if (element-of-set? (car set2) set3)
;               (union set1 (cdr set2) set3)
;               (union set1 (cdr set2) (cons (car set2) set3))))
;          (else set3)))
;  (union set1 set2 '()))

;(define (intersection-set set1 set2)
;  (define (intersection set1 set2 set3)
;    (cond ((or (null? set1) (null? set2)) set3)
;          ((and (element-of-set? (car set1) set2) (not (element-of-set? (car set1) set3)))
;           (intersection (cdr set1) set2 (cons (car set1) set3)))
;          (else (intersection (cdr set1) set2 set3))))
;  (intersection set1 set2 '()))

;(union-set '(1 1 2 2 3 3) '(2 2 3 3 4 4 5))
;(intersection-set '(1 2 2 3 3) '(2 2 3 3 4 5))

(define (adjoin-set x set)
  (cond ((or (null? set) (< x (car set))) (cons x set))
        ((= x (car set)) set)
        (else (cons (car set) (adjoin-set x (cdr set))))))

;(adjoin-set 6 '(1 3 4 5))

;(define (union-set set1 set2)
;  (cond ((null? set1) set2)
;        ((null? set2) set1)
;        (else (let ((x1 (car set1)) (x2 (car set2)))
;                (cond ((= x1 x2)
;                       (cons x1 (union-set (cdr set1) (cdr set2))))
;                      ((< x1 x2)
;                       (cons x1 (union-set (cdr set1) set2)))
;                      ((< x2 x1)
;                       (cons x2 (union-set set1 (cdr set2)))))))))

;(union-set '(1 2 3) '(2 3 4 5))

(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append (tree->list-1 (left-branch tree))
              (cons (entry tree) (tree->list-1 (right-branch tree))))))

; (1 3 5 7 9 11) for all
; O(nlogn)

(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree) (copy-to-list (right-branch tree) result-list)))))
  (copy-to-list tree '()))

; (1 3 5 7 9 11) for all
; O(n)

(define (list->tree elements)
  (car (partial-tree elements (length elements))))
(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result
               (partial-tree elts left-size)))
          (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result))
                (right-size (- n (+ left-size 1))))
            (let ((this-entry (car non-left-elts))
                  (right-result
                   (partial-tree
                    (cdr non-left-elts)
                    right-size)))
              (let ((right-tree (car right-result))
                    (remaining-elts
                     (cdr right-result)))
                (cons (make-tree this-entry
                                 left-tree
                                 right-tree)
                      remaining-elts))))))))

; partial-tree splits the list into half with the entry in the middle, biasing towards the right when there
; are an even number of elements in the list. It gets the balanced tree of the left and the remaining
; elements at that point, takes the entry to be the car of that list and gets the balanced tree of
; the right and the remaining elements. It then puts the entry, left tree and right tree into a tree, and
; returns the pair of the tree and the remaining elements.
; O(n)

(define (union-set set1 set2)
  (define (union set1 set2)
    (cond ((null? set1) set2)
          ((null? set2) set1)
          (else (let ((x1 (car set1)) (x2 (car set2)))
                  (cond ((= x1 x2)
                         (cons x1 (union (cdr set1) (cdr set2))))
                        ((< x1 x2)
                         (cons x1 (union (cdr set1) set2)))
                        ((< x2 x1)
                         (cons x2 (union set1 (cdr set2)))))))))
  (list->tree (union (tree->list-2 set1) (tree->list-2 set2))))

(define (intersection-set set1 set2)
  (define (intersection set1 set2)
    (if (or (null? set1) (null? set2))
        '()
        (let ((x1 (car set1)) (x2 (car set2)))
          (cond ((= x1 x2)
                 (cons x1 (intersection (cdr set1)
                                            (cdr set2))))
                ((< x1 x2)
                 (intersection (cdr set1) set2))
                ((< x2 x1)
                 (intersection set1 (cdr set2)))))))
  (list->tree (intersection (tree->list-2 set1) (tree->list-2 set2))))

;(union-set (list->tree '(1 3 5)) (list->tree '(4 5 6)))
;(intersection-set (list->tree '(1 3 5)) (list->tree '(4 5 6)))

(define (lookup given-key set-of-records)
  (cond ((null? set-of-records) false)
        ((= given-key (key (car set-of-records))) (car set-of-records))
        ((< given-key (key (car set-of-records)))
         (lookup given-key (left-branch set-of-records)))
        ((> given-key (key (car set-of-records)))
         (lookup given-key (right-branch set-of-records)))))
