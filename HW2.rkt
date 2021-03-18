#lang racket
; Josh Levy

; #1
; dotproduct takes a two vectors (lists of numbers) and computes the dot product of the vectors
; If one list is longer than the other, you can ignore the extra numbers of the longer list
(define dotproduct
  (lambda (v1 v2 return)
    (cond
      [(or (null? v1) (null? v2)) (return 0)]
      [else (dotproduct (cdr v1) (cdr v2) (lambda (v) (return (+ (* (car v1) (car v2)) v))))])))

; #2
; removesubsequence takes two lists of atoms.
; The first list is a subsequence of the second list. The method should return the second list with the first occurrence of the subsequence removed. 
(define removesubsequence
  (lambda (seq lis return)
    (cond
      [(or (null? seq) (null? lis)) (return lis)]
      [(eq? (car seq) (car lis)) (removesubsequence (cdr seq) (cdr lis) (lambda (v) (return v)))]
      [else (removesubsequence seq (cdr lis) (lambda (v) (return (cons (car lis) v))))])))

; #3
; squareroot takes two numbers, a value and an iteration.
; The iteration will be an integer greater than or equal to 0.
; The method will compute the squareroot of the value using iteration rounds of Newton's method, starting with an initial value equal to the input value
(define squareroot
  (lambda (val iter return)
    (cond
      [(eq? iter 0) (return val)]
      [else (squareroot val (- iter 1) (lambda (v) (return (- v (/ (- (* v v) val) (* 2 v))))))])))

; #4
; numatoms* takes a nested list and returns the number of atoms (not including the empty list) in the list
(define numatoms*
  (lambda (lis return)
    (cond
      [(null? lis) (return 0)]
      [(pair? lis) (numatoms* (car lis) (lambda (v1) (numatoms* (cdr lis) (lambda (v2) (return (+ v1 v2))))))]
      [else (return 1)])))

; #5
; reverse* takes a nested list and reverses the contents of the list and all nested lists
(define reverse*
  (lambda (lis return)
    (cond
      [(null? lis) (return lis)]
      [(list? (car lis)) (reverse* (car lis) (lambda (v1) (reverse* (cdr lis) (lambda (v2) (return (append v2 (cons v1 '())))))))]
      [else (reverse* (cdr lis) (lambda (v) (return (append v (cons (car lis) '())))))])))

;#6
; maps a function onto a list
(define mymap
  (lambda (func lis return)
    (cond
      [(null? lis) (return lis)]
      [else (mymap func (cdr lis) (lambda (v) (return (cons (func (car lis)) v))))])))
; vectormult takes a row vector (a list of numbers) and matrix (a list of lists of numbers) and multiplies the vector times the matrix.
; The result is a vector where the ith element of the result is the dotproduct of the input vector and the ith column of the matrix
(define vectormult
  (lambda (vec mat return)
    (cond
      [(or (null? vec) (null? mat)) (return '())]
      [(null? (car mat)) (return '())]     
      [else (vectormult vec (mymap cdr mat (lambda (v1) v1)) (lambda (v2) (return (cons (dotproduct vec (mymap car mat (lambda (v3) v3)) (lambda (v4) v4)) v2))))])))

; #7
; matrixmultiply takes two matrices (a list of lists of numbers) and multiplies them
(define matrixmultiply
  (lambda (mat1 mat2 return)
    (cond
      [(or (null? mat1) (null? mat2)) (return '())]
      [else (matrixmultiply (cdr mat1) mat2 (lambda (v1) (return (cons (vectormult (car mat1) mat2 (lambda (v2) v2)) v1))))])))

; #8
; removesubsequence* takes a list of atoms and a general list. The first list is a subsequence of the second list.
; The method should return the second list with the first occurrence of the subsequence removed
(define removesubsequence*
  (lambda (seq lis)
    (removesubsequence*-cps seq lis (lambda (v1 v2) v2))))

(define removesubsequence*-cps
  (lambda (seq lis return)
    (cond
      [(or (null? seq) (null? lis)) (return seq lis)]
      [(eq? (car seq) (car lis)) (removesubsequence*-cps (cdr seq) (cdr lis) (lambda (v1 v2) (return v1 v2)))]
      [(list? (car lis)) (removesubsequence*-cps seq (car lis) (lambda (v1 v2) (removesubsequence*-cps v1 (cdr lis) (lambda (v3 v4) (return v3 (cons v2 v4))))))]
      [else (removesubsequence*-cps seq (cdr lis) (lambda (v1 v2) (return v1 (cons (car lis) v2))))])))

; #9
; suffix takes an atom and a list and returns a list containing all elements that occur after the last occurrence of the atom
(define suffix
  (lambda (atom lis)
    (call/cc
     (lambda (break)
       (letrec ((proc (lambda (ls return)
                        (cond
                          [(null? ls) (return '())]
                          [(eq? atom (car ls)) (break (proc (cdr ls) (lambda (v) v)))]
                          [else (proc (cdr ls) (lambda (v) (return (cons (car ls) v))))]))))
                (proc lis (lambda (v) v)))))))                                   
                     
; #10
; xindex takes an atom and a list containing sublists.
; The output list should be the same as the input list except that any sublist (including the main list) that contains the given atom should be emptied of all contents (atoms, lists, etc.),
; and instead, the only content of that sublist should be the index of the first occurrence of the atom in that list
(define xindex
  (lambda (atom lis)
    (call/cc (lambda (return) (xindex-cps atom lis (lambda (v) (return v)))))))

(define xindex-cps
  (lambda (atom lis return)
    (cond
      [(null? lis) lis]
      [(list? (car lis)) (cons (xindex atom (car lis)) (xindex-cps atom (cdr lis) (lambda (v) (return (cons (+ 1 (car v)) '())))))]
      [(eq? atom (car lis)) (return '(1))]
      [else (cons (car lis) (xindex-cps atom (cdr lis) (lambda (v) (return (cons (+ 1 (car v)) '())))))])))