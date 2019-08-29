#lang sicp

;(define (make-table same-key?)
;  (let ((local-table (list '*table*)))
;    (define (lookup key-1 key-2)
;      (let ((subtable
;             (assoc key-1 (cdr local-table) same-key?)))
;        (if subtable
;            (let ((record
;                   (assoc key-2 (cdr subtable) same-key?)))
;              (if record (cdr record) false))
;            false)))
;    (define (insert! key-1 key-2 value)
;      (let ((subtable
;             (assoc key-1 (cdr local-table) same-key?)))
;        (if subtable
;            (let ((record
;                   (assoc key-2 (cdr subtable) same-key?)))
;              (if record
;                  (set-cdr! record value)
;                  (set-cdr! subtable
;                            (cons (cons key-2 value)
;                                  (cdr subtable)))))
;            (set-cdr! local-table
;                      (cons (list key-1 (cons key-2 value))
;                            (cdr local-table)))))
;      'ok)
;    (define (dispatch m)
;      (cond ((eq? m 'lookup-proc) lookup)
;            ((eq? m 'insert-proc!) insert!)
;            (else (error "Unknown operation: TABLE" m))))
;    dispatch))
;(define operation-table (make-table equal?))
;(define get (operation-table 'lookup-proc))
;(define put (operation-table 'insert-proc!))

;(define (assoc key records same-key?)
;  (cond ((null? records) false)
;        ((same-key? key (caar records)) (car records))
;        (else (assoc key (cdr records)))))

;(put 'abc 'abc 1)
;(get 'abc 'abc)

;(define (make-table) (list '*table*))

; Find a table or record
;(define (assoc key records table?)
;  (cond ((null? records) false)
;        ((and (or (and table? (pair? (cdar records)))
;                  (and (not table?) (not (pair? (cdar records)))))
;              (equal? key (caar records)))
;         (car records))
;        (else (assoc key (cdr records) table?))))

; Get the subtable or record, depending on whether this is the last key
  ; If the subtable or record exists
    ; If this is the last key, return the value of the record
    ; If this is not the last key, lookup the rest of the keys in the subtable
  ; If the subtable or record does not exist, return false
;(define (lookup keys table)
;  (let ((rest-of-keys (cdr keys)))
;    (let ((last-key? (null? rest-of-keys)))
;      (let ((subtable (assoc (car keys) (cdr table) (not last-key?))))
;        (if subtable
;            (if last-key?
;                (cdr subtable)
;                (lookup rest-of-keys subtable))
;            false)))))

; Get the subtable or record, depending on whether this is the last key
  ; If the subtable or record exists
    ; If this is the last key, modify the value of the record
    ; If this is not the last key, insert the rest of the keys in the subtable
  ; If the record does not exist, insert the record in the current table
  ; If the subtable does not exist, insert the record nested in subtables in the current table
;(define (insert! keys value table)
;  (let ((rest-of-keys (cdr keys)))
;    (let ((last-key? (null? rest-of-keys)))
;      (let ((subtable (assoc (car keys) (cdr table) (not last-key?))))
;        (if subtable
;            (if last-key?
;                (set-cdr! subtable value)
;                (insert! rest-of-keys value subtable))
;            (if last-key?
;                (set-cdr! table
;                          (cons (cons (car keys) value)
;                                (cdr table)))
;                (let ((new-table (list (car keys))))
;                  (set-cdr! table
;                            (cons new-table
;                                  (cdr table)))
;                  (insert! rest-of-keys value new-table)))))))
;  'ok)

(define (key tree) (car tree))
(define (value tree) (cadr tree))
(define (left-branch tree) (caddr tree))
(define (right-branch tree) (cadddr tree))
(define (set-value! tree value) (set-car! (cdr tree) value))
(define (set-left-branch! tree branch) (set-car! (cddr tree) branch))
(define (set-right-branch! tree branch) (set-car! (cdddr tree) branch))
(define (make-tree key value left right) (list key value left right))

; In order to use recursion, there has to be an additional parameter (local variable) for the current tree
; But that means set! on the local variable won't work, so make-table has to start with an existing tree
(define (make-table k v) (make-tree k v '() '()))
(define (lookup k table)
  (cond ((null? table) false)
        ((equal? k (key table)) (value table))
        ((< k (key table)) (lookup k (left-branch table)))
        ((> k (key table)) (lookup k (right-branch table)))))
(define (insert! k v table)
  (cond ((null? table) (set! table (make-tree k v '() '())))
        ((equal? k (key table)) (set-value! table v))
        ((< k (key table))
         (if (null? (left-branch table))
             (set-left-branch! table (make-tree k v '() '()))
             (insert! k v (left-branch table))))
        ((> k (key table))
         (if (null? (right-branch table))
             (set-right-branch! table (make-tree k v '() '()))
             (insert! k v (right-branch table)))))
  'ok)

(define test (make-table 2 3))
(lookup 1 test)
(insert! 2 4 test)
(lookup 2 test)
(insert! 3 2 test)
(lookup 3 test)

; (memoize fib) will call (fib (- n 1)) and (fib (- n 2)), which are non-memoized versions