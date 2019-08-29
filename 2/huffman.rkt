#lang sicp

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define (make-leaf symbol weight) (list 'leaf symbol weight))
(define (leaf? object) (eq? (car object) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))
(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))
(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))
(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch
               (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))
(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit: CHOOSE-BRANCH" bit))))

(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                   (make-leaf 'B 2)
                   (make-code-tree
                    (make-leaf 'D 1)
                    (make-leaf 'C 1)))))
(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

;(decode sample-message sample-tree) ; ADABBCA

(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))
(define (encode-symbol symbol tree)
  (cond ((or (null? tree) (not (element-of-set? symbol (symbols tree))))
         (error "bad symbol: ENCODE-SYMBOL" symbol))
        ((and (leaf? (left-branch tree)) (eq? symbol (symbol-leaf (left-branch tree))))
         (list 0))
        ((and (leaf? (right-branch tree)) (eq? symbol (symbol-leaf (right-branch tree))))
         (list 1))
        ((and (not (leaf? (left-branch tree))) (element-of-set? symbol (symbols (left-branch tree))))
         (cons 0 (encode-symbol symbol (left-branch tree))))
        ((and (not (leaf? (right-branch tree))) (element-of-set? symbol (symbols (right-branch tree))))
         (cons 1 (encode-symbol symbol (right-branch tree))))))

;(encode '(A D A B B C A) sample-tree)

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))
(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair) ; symbol
                               (cadr pair)) ; frequency
                    (make-leaf-set (cdr pairs))))))

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))
(define (successive-merge set)
  (if (= (length set) 1)
      (car set)
      (successive-merge (adjoin-set (make-code-tree (car set) (cadr set)) (cddr set)))))

(encode '(GET A JOB
              SHA NA NA NA NA NA NA NA NA
              GET A JOB
              SHA NA NA NA NA NA NA NA NA
              WAH YIP YIP YIP YIP YIP YIP YIP YIP YIP
              SHA BOOM)
        (generate-huffman-tree '((A 2) (BOOM 1) (GET 2) (JOB 2) (SHA 3) (NA 16) (WAH 1) (YIP 9))))
; 84 vs. 108
; 1, n - 1

; For balanced tree, O(n)
; O(1), O(n^2)