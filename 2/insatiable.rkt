#lang sicp

(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datum: TYPE-TAG" datum)))
(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Bad tagged datum: CONTENTS" datum)))
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (error
           "No method for these types: APPLY-GENERIC"
           (list op type-tags))))))

; The type of name (symbols, huffman codes) should be attached to the name
; The type of set (unordered, ordered, binary tree) should be attached to the division's file
(define (get-record name records)
  (apply-generic 'lookup name records))

; The type of record should be attached to the employee's record
(define (get-salary record)
  (apply-generic 'salary record))

(define (find-employee-record name records)
  (if (null? records)
      false
      (let ((try (get-record name (car records))))
        (if try
            try
            (find-employee-record name (cdr records))))))

; If the division or employee records of the new company are structured in a new way, new procedures
; (lookup, address, salary, etc.) must be created and put into the table