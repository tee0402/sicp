#lang sicp

; (a b c)
; ((george))
; ((y1 y2))
; (y1 y2)
; false
; false
; (red shoes blue socks)

(define (equal? a b)
  (cond ((and (not (pair? a)) (not (pair? b))) (eq? a b))
        ((and (pair? a) (pair? b)) (and (equal? (car a) (car b)) (equal? (cdr a) (cdr b))))
        (else false)))

; (car ''abracadabra) is interpreted as (car (quote (quote abracadabra))), and when the first quote special form is evaluated, the
; interpreter will interpret (quote abracadabra) as a list, the car of which is quote

(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))
(define (=number? exp num) (and (number? exp) (= exp num)))
;(define (make-sum a1 a2)
;  (cond ((=number? a1 0) a2)
;        ((=number? a2 0) a1)
;        ((and (number? a1) (number? a2))
;         (+ a1 a2))
;        (else (list '+ a1 a2))))
;(define (sum? x) (and (pair? x) (eq? (car x) '+)))
;(define (addend s) (cadr s))
;(define (augend s) (caddr s))
;(define (make-product m1 m2)
;  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
;        ((=number? m1 1) m2)
;        ((=number? m2 1) m1)
;        ((and (number? m1) (number? m2)) (* m1 m2))
;        (else (list '* m1 m2))))
;(define (product? x) (and (pair? x) (eq? (car x) '*)))
;(define (multiplier p) (cadr p))
;(define (multiplicand p) (caddr p))
(define (make-exponentiation b e)
  (cond ((=number? e 0) 1)
        ((=number? e 1) b)
        (else (list '** b e))))
(define (exponentiation? x) (and (pair? x) (eq? (car x) '**)))
(define (base e) (cadr e))
(define (exponent e) (caddr e))
(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        ((sum? exp) (make-sum (deriv (addend exp) var)
                              (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (deriv (multiplier exp) var)
                        (multiplicand exp))))
        ((exponentiation? exp)
         (make-product
          (make-product (exponent exp)
                        (make-exponentiation (base exp) (- (exponent exp) 1)))
          (deriv (base exp) var)))
        (else
         (error "unknown expression type: DERIV" exp))))

; Use the associative property of addition and multiplication: x+y+z becomes x+(y+z) and xyz becomes x(yz)
;(define (augend s)
;  (if (= (length s) 3)
;      (caddr s)
;      (cons '+ (cddr s))))
;(define (multiplicand p)
;  (if (= (length p) 3)
;      (caddr p)
;      (cons '* (cddr p))))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2))
         (+ a1 a2))
        (else (list a1 '+ a2))))
;(define (sum? x) (and (pair? x) (eq? (cadr x) '+)))
;(define (addend s) (car s))
(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list m1 '* m2))))
;(define (product? x) (and (pair? x) (eq? (cadr x) '*)))
(define (multiplier p) (car p))

(define (anti-memq item x)
  (cond ((null? x) false)
        ((eq? item (car x)) '())
        (else (cons (car x) (anti-memq item (cdr x))))))
(define (anti-memq-2 item x)
  (cond ((null? (cddr x)) false)
        ((eq? item (caddr x)) '())
        (else (cons (car x) (anti-memq-2 item (cdr x))))))
(define (memq+1 item x)
  (cond ((null? (cdr x)) false)
        ((eq? item (cadr x)) x)
        (else (memq+1 item (cdr x)))))
(define (remove-list x)
  (if (and (pair? x) (= (length x) 1)) (car x) x))
; Addition (has at least one +):
; If starting with a +,
;   If there is a *, the addend is everything before the + before the first * expression and the augend is everything from the first * expression
;   If no * and all +s, the addend is the car and the augend is the rest
; If starting with a *, the addend is everything before the first + and the augend is everything after the first +
(define (sum? x) (and (pair? x) (memq '+ x)))
(define (addend s)
  (if (not (memq '* s))
      (car s)
      (if (eq? (cadr s) '+)
          (remove-list (anti-memq-2 '* s))
          (anti-memq '+ s))))
(define (augend s)
  (if (not (memq '* s))
      (remove-list (cddr s))
      (if (eq? (cadr s) '+)
          (memq+1 '* s)
          (remove-list (cdr (memq '+ s))))))
; Multiplication (no +, all *s):
; The addend is the car and the augend is the rest
(define (product? x) (and (pair? x) (not (memq '+ x))))
(define (multiplicand p) (remove-list (cddr p)))

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

; theta(1), much faster than theta(n)
;(define (adjoin-set x set) (cons x set))
; 2n^2 steps, theta(n^2), we iterate through both sets instead of just set1
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
; 2n^2 steps, theta(n^2), does element-of-set? check for each element of set1 in both set2 and set3
;(define (intersection-set set1 set2)
;  (define (intersection set1 set2 set3)
;    (cond ((or (null? set1) (null? set2)) set3)
;          ((and (element-of-set? (car set1) set2) (not (element-of-set? (car set1) set3)))
;           (intersection (cdr set1) set2 (cons (car set1) set3)))
;          (else (intersection (cdr set1) set2 set3))))
;  (intersection set1 set2 '()))
; I would prefer this representation if adjoin-set is time-critical or the program consists of a lot of adjoin-set operations

;(define (adjoin-set x set)
;  (cond ((or (null? set) (< x (car set))) (cons x set))
;        ((= x (car set)) set)
;        (else (cons (car set) (adjoin-set x (cdr set))))))

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

; Both produce the same result for every tree, which is its in-order traversal, which is (1 3 5 7 9 11) for the trees in Figure 2.16
; The first adds elements to the list in ascending order and the second adds elements to the list in descending order
; The two procedures do not have the same order of growth: the second has slower growth (theta(n)) than the first (theta(nlogn))
; This is because the first uses append, whose number of steps grows with the size of the first list, to append the left subtree
; In a balanced tree, the left subtree will have about half of the elements in the tree
; Appending a left subtree with more than one element will append elements that have already been appended in the lower levels of
; the tree, leading to duplicated operations and growth larger than theta(n)

; partial-tree splits the list into half with the entry in the middle, biasing towards the right when there
; is an even number of elements in the list. It creates the left balanced subtree and gets the remaining elements
; not in the left subtree, takes the entry to be the car of those remaining elements, creates the right balanced
; subtree and gets the remaining elements. It then creates the tree from the entry, left subtree, and right subtree,
; and returns the pair of the tree and the remaining elements
;(make-tree 5 (make-tree 1 '() (make-tree 3 '() '())) (make-tree 9 (make-tree 7 '() '()) (make-tree 11 '() '())))
; list->tree grows at theta(n) since length grows at theta(n) and partial-tree goes through each element of the list once to build
; a node, so it also grows at theta(n)

(define (entry tree) (car tree))
;(define (left-branch tree) (cadr tree))
;(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))
(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list
                             (right-branch tree)
                             result-list)))))
  (copy-to-list tree '()))
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

;(define (lookup given-key set-of-records)
;  (cond ((null? set-of-records) false)
;        ((= given-key (key (car set-of-records))) (car set-of-records))
;        ((< given-key (key (car set-of-records)))
;         (lookup given-key (left-branch set-of-records)))
;        ((> given-key (key (car set-of-records)))
;         (lookup given-key (right-branch set-of-records)))))

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
(decode sample-message sample-tree)
; (A D A B B C A)

(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))
(define (encode-symbol symbol tree)
  (define (encode-1 current-branch)
    (let ((left (left-branch current-branch))
          (right (right-branch current-branch)))
      (cond ((and (leaf? left) (eq? symbol (symbol-leaf left)))
             (list 0))
            ((and (leaf? right) (eq? symbol (symbol-leaf right)))
             (list 1))
            ((and (not (leaf? left)) (memq symbol (symbols left)))
             (cons 0 (encode-1 left)))
            ((and (not (leaf? right)) (memq symbol (symbols right)))
             (cons 1 (encode-1 right))))))
  (if (memq symbol (symbols tree))
      (encode-1 tree)
      (error "symbol not in tree: ENCODE-SYMBOL" symbol)))
(encode '(A D A B B C A) sample-tree)
; (0 1 1 0 0 1 0 1 0 1 1 1 0)

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
; 84 bits are required for the encoding, at least 108 bits would be required if we used a fixed-length code

; The tree will consist of leaves on the left branches and subtrees on the right branches, until a leaf is reached
; 1 bit is required to encode the most frequent symbol and n - 1 bits are required to encode the least frequent symbol

; For a balanced tree, the order of growth in the number of steps needed to encode a symbol is theta(n), since the number of elements
; roughly halves every time we branch down, the combined sizes of the symbol lists we search at each level will be n, n/2, n/4, ..., 1,
; which add up to approximately 2n, or theta(n)
; For the special case above, the order of growth is theta(n) to encode the most frequent symbol, since we have to first search
; the symbol list at the root for it, and theta(n^2) to encode the least frequent symbol, since we search the symbol lists of sizes
; n, n-1, n-2, ..., 1 consecutively, which add up to n(n+1)/2, or theta(n^2)