#lang racket
; Josh Levy

; #1
; inorder? takes a list of numbers and returns #t if the numbers are in non-decreasing order
(define inorder?
  (lambda (lis)
    (cond
      [(or (null? lis) (null? (cdr lis))) #t]
      [(< (car lis) (cadr lis)) (inorder? (cdr lis))]
      [else #f])))

; #2
; dotproduct takes a two vectors (lists of numbers) and computes the dot product of the vectors
; If one list is longer than the other, you can ignore the extra numbers of the longer list
(define dotproduct
  (lambda (lis1 lis2)
    (cond
      [(or (null? lis1) (null? lis2)) 0]
      [else (+ (* (car lis1) (car lis2)) (dotproduct (cdr lis1) (cdr lis2)))])))

; #3
; squareroot takes two numbers, a value and an iteration.
; The iteration will be an integer greater than or equal to 0.
; The method will compute the squareroot of the value using iteration rounds of Newton's method, starting with an initial value equal to the input value
(define squareroot
  (lambda (val iter)
    (cond
      [(zero? iter) val]
      [else (- (squareroot val (- iter 1)) (/ (- (* (squareroot val (- iter 1)) (squareroot val (- iter 1))) val) (* 2 (squareroot val (- iter 1)))))])))

; #4
; removesubsequence takes two lists of atoms.
; The first list is a subsequence of the second list.
; The method should return the second list with the first occurence of the subsequence removed.
(define removesubsequence
  (lambda (seq lis)
    (cond
      [(null? seq) lis]
      [(eq? (car seq) (car lis)) (removesubsequence (cdr seq) (cdr lis))]
      [else (cons (car lis) (removesubsequence seq (cdr lis)))])))

; #5
; reverse* takes a nested list and reverses the contents of the list and all nested lists
(define reverse*
  (lambda (lis)
    (cond
      [(null? lis) lis]
      [(list? (car lis)) (append (reverse* (cdr lis)) (cons (reverse* (car lis)) '()))]
      [else (append (reverse* (cdr lis)) (cons (car lis) '()))])))
; #6
; first* takes a list of lists and returns the first (left most) atom that appears in the list, regardless of how nested it is
(define first*
  (lambda (lis)
    (cond
      [(null? lis) lis]
      [(pair? lis) (first* (car lis))]
      [else lis])))

; #7
; returns the last element in a list
(define last*-helper
  (lambda (lis)
    (cond
      [(null? lis) lis]
      [(null? (cdr lis)) (car lis)]
      [else (last*-helper (cdr lis))])))
; last* takes a list of lists and returns the last (right most) atom that appears in the list, regardless of how nested it is.
(define last*
  (lambda (lis)
    (cond
      [(or (null? lis) (not (pair? lis))) lis]
      [else (last* (last*-helper lis))])))

; #8
; adds the number in a list that can contain nested lists
(define sum*
    (lambda (lis)
      (cond
        [(null? lis) 0]
        [(number? lis) lis]
        [(list? (car lis)) (+ (sum* (car lis)) (sum* (cdr lis)))]
        [else (+ (car lis) (sum* (cdr lis)))])))
; numorder*? takes a possibly nested list of numbers, and returns #t if the values of the entries in the list and all sublists are in non-decreasing order.
; The value of a number is the number. The value of a list is the sum of the values in that list
(define numorder*?
  (lambda (lis)
    (cond
      [(or (null? lis) (null? (cdr lis))) #t]
      [(list? (car lis)) (and (numorder*? (cdr lis)) (and (numorder*? (car lis)) (<= (sum* (car lis)) (sum* (cadr lis)))))]
      [else  (and (numorder*? (cdr lis)) (<= (sum* (car lis)) (sum* (cadr lis))))])))

; #9
; maps a function onto a list
(define mymap
  (lambda (func lis)
    (cond
      [(null? lis) lis]
      [else (pair? (car lis)) (cons (func (car lis)) (mymap func (cdr lis)))])))
; vectormult takes a row vector (a list of numbers) and matrix (a list of lists of numbers) and multiplies the vector times the matrix.
; The result is a vector where the ith element of the result is the dotproduct of the input vector and the ith column of the matrix
(define vectormult
  (lambda (vec mat)
    (cond
      [(null? (car mat)) '()]     
      [else (pair? (car mat)) (cons (dotproduct vec (mymap car mat)) (vectormult vec (mymap cdr mat)))])))

; #10
; matrixmultiply takes two matrices (a list of lists of numbers) and multiplies them.
(define matrixmultiply
  (lambda (m1 m2)
    (cond
      [(or (null? m1) (null? m2)) '()]
      [else (cons (vectormult (car m1) m2) (matrixmultiply (cdr m1) m2))])))