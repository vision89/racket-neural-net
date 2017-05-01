#lang racket
; Author: Dustin Gulley
; Class: CSC 525
; Description: Project 4. Represent a matrix and provide accessors/mutators
; Due date: April 26 2017

; A small language for using matrixes
(define matrix-lang
  (letrec (


           ; *** Parsing Logic ***

           ; s-expr -> s-expr
           ; Checks if the parameter matches the pattern required to be a matrix.  If so, it returns it, if not it returns null
           ; given (1 2 (1 2)), expect (1 2 (1 2))
           ; given (1 2 3 (1 2)), expect null
           (is-prototype?
            (lambda x
              (cond ((and (eq? (length x) 3) (number? (car x)) (number? (cadr x))) x)
                    (else (quote ())))))

           ; number -> number
           ; Checks if the parameter is a number >= 0.  If so it returns it, if not it returns null.
           ; given 1, expect 1
           ; given -1, expect -1
           (num>=0?
            (lambda (x)
              (cond ((and (number? x) (>= x 0) x))
                    (else (quote ())))))

           ; list -> list
           ; Checks if the parameter is a number >= 0.  If so it returns it, if not it returns null.
           ; given 1, expect 1
           ; given -1, expect -1
           (list-of-numbers?
            (lambda (a-lon)
              (let/cc skip
                (cond ((null? a-lon) (quote ()))
                      ((number? (car a-lon)) (cons (car a-lon) (list-of-numbers? (cdr a-lon))))
                      (else (skip (quote ())))))))

           ; list -> list
           ; Checks if the parameter is a matrix, if so it returns it, if not it returns null
           ; given 1, expect 1
           ; given -1, expect -1
           (matrix?
            (lambda (x)
              (cond ((and (eq? (length x) 3) (is-prototype? (car x) (cadr x) (caddr x)) (is-prototype? (car x) (cadr x) (caddr x))) x)
                    (else (quote ())))))

           ; list -> bool
           ; Checks if the parameter is a matrix, if so it returns true, if not it returns false
           ; given 1, expect 1
           ; given -1, expect -1
           (is-matrix?
            (lambda x
              (cond ((matrix? x) #t)
                    (else #f))))
 

           ; *** matrix logic ****

           ; atom atom atom -> matrix
           ; Checks if the parameters match the matrix pattern,
           ; if so it creates a matrix from them,
           ; if not it returns an empty matrix
           ; given 1 2 (2 2) expect (1 2 (2 2))
           ; given 1 2 3 4 5, expect (0 0 ())
           (create-matrix
            (match-lambda*
              ((list (? number? x) (? number? y) (? list? z)) (list x y z))
              ((list (? number? x) (? list? y)) (list x x y))
              (_ (list 0 0 '(quote ())))))

           ; matrix -> number
           ; Returns the number of rows in the matrix
           ; given (1 2 (2 2)) expect 1
           ; given (0 0 ()), expect 0
           (row-count
            (lambda (x)
              (car x)))

           ; matrix -> number
           ; Returns the number of columns in the matrix
           ; given (1 2 (2 2)) expect 2
           ; given (0 0 ()), expect 0
           (col-count
            (lambda (x)
              (cadr x)))

           ; matrix -> list
           ; Returns the matrix asa a list
           ; given (1 2 (2 2)) expect (2 2)
           ; given (0 0 ()), expect ()
           (as-list
            (lambda (x)
              (caddr x)))

           ; matrix number -> list
           ; Returns the specified row from the matrix
           ; with matrix 1 3 1, 2 2 3, 3 1 2, given 2, expect (2 2 3)
           ; with matrix 1 2 3, 3 2 1, 1 3 2, given 3, expect (1 3 2)
           ; with matrix 1 3 1, 2 2 3, 3 1 2, given -1, expect ()
           ; with matrix 1 3 1, 2 2 3, 3 1 2, given 0, expect ()
           ; with matrix 1 3 1, 2 2 3, 3 1 2, given 4, expect ()
           (get-row (lambda (a-matrix row-num)
                      (let ((total-rows (row-count a-matrix))
                            (total-cols (col-count a-matrix)))
                        (letrec ((get-row-h (lambda (rows cols a-list)
                                              (cond ((> cols total-cols) (get-row-h (+ rows 1) 1 a-list))
                                                    ((or (> rows total-rows) (null? a-list)) (quote ()))
                                                    ((= rows row-num) (cons (car a-list) (get-row-h rows (+ cols 1) (cdr a-list))))
                                                    (else (get-row-h rows (+ cols 1) (cdr a-list)))))))
                          (get-row-h 1 1 (as-list a-matrix))))))

           ; matrix number -> list
           ; Returns the specified column from the matrix
           ; with matrix 1 3 1, 2 2 3, 3 1 2, given 2, expect (3 2 1)
           ; with matrix 1 2 3, 3 2 1, 1 3 2, given 3, expect (3 1 2)
           ; with matrix 1 3 1, 2 2 3, 3 1 2, given -1, expect ()
           ; with matrix 1 3 1, 2 2 3, 3 1 2, given 0, expect ()
           ; with matrix 1 3 1, 2 2 3, 3 1 2, given 4, expect ()
           (get-col (lambda (a-matrix col-num)
                      (let ((total-rows (row-count a-matrix))
                            (total-cols (col-count a-matrix)))
                        (letrec ((get-col-h (lambda (rows cols a-list)
                                              (cond ((> cols total-cols) (get-col-h (+ rows 1) 1 a-list))
                                                    ((or (> rows total-rows) (null? a-list)) (quote ()))
                                                    ((= cols col-num) (cons (car a-list) (get-col-h rows (+ cols 1) (cdr a-list))))
                                                    (else (get-col-h rows (+ cols 1) (cdr a-list)))))))
                          (get-col-h 1 1 (as-list a-matrix))))))

           ; list -> list
           ; Multiplies two matrixs.  It does no mutation but instead returns a list of the the values of the multiplied matrixes
           ; Note the both matrixes are expected to have matching the row count match the col count of the other,
           ; If they don't fit this format.. problems will happen
           ; given (1 3 1 2 2 3 3 1 2) * (1 2 3 3 2 1 1 3 2), expect (11 11 8 11 17 14 8 14 14)
           (multiply (lambda (a-matrix b-matrix)
                       (let (
                             (a-list (as-list a-matrix))
                             (b-list (as-list b-matrix))
                             (total-rows (row-count a-matrix))
                             (total-cols (col-count b-matrix))
                             )
                         (letrec (
                                  (multiple-add-lists (lambda (a-list b-list)
                                                        (cond ((or (null? a-list) (null? b-list)) 0)
                                                              (else (+ (* (car a-list) (car b-list)) (multiple-add-lists (cdr a-list) (cdr b-list)))))))
                                  (multiply-h
                                   (lambda (row-n col-n)
                                     (let ((row (get-row a-matrix row-n))
                                           (col (get-col b-matrix col-n)))
                                       (cond ((> row-n total-rows) (quote ()))
                                             ((> col-n total-cols) (multiply-h (+ 1 row-n) 1))
                                             (else (cons (multiple-add-lists row col) (multiply-h row-n (+ col-n 1)))))))))
                           (create-matrix total-rows total-cols (multiply-h 1 1))))))

           ; function matrix -> matrix
           ; Applies a function to every element of a matrix
           ; given (lambda (x) (+ 1 x)) (1 2 (2 2)) expect (1 2 (3 3))
           (apply-each
            (lambda (a-func a-matrix)
              (letrec ((apply-each-h (lambda (matrix-list)
                                       (cond ((null? matrix-list) (quote ()))
                                             (else (cons (a-func (car matrix-list)) (apply-each-h (cdr matrix-list))))))))
                (create-matrix (row-count a-matrix) (col-count a-matrix) (apply-each-h (as-list a-matrix))))))

           ; _ -> number
           ; Retrives the number of nodes within the matrix
           ; with matrix 1 3 1, 2 2 3, 3 1 2, expect 9
           (count
            (lambda (a-matrix)
              (* (row-count a-matrix) (col-count a-matrix))))

           ; matrix matrix -> matrix
           ; Adds two matrixes
           ; given (1 2 (3 3)) (1 2 (2 2)) expect (1 2 (5 5))
           (add
            (lambda (a-matrix b-matrix)
              (letrec ((add-h
                        (lambda (a-list b-list)
                          (cond ((or (null? a-list) (null? b-list)) (quote ()))
                                (else (cons (+ (car a-list) (car b-list)) (add-h (cdr a-list) (cdr b-list))))))))
                (let ((answer (add-h (as-list a-matrix) (as-list b-matrix))))
                  (cond ((not (= (* (row-count a-matrix) (col-count b-matrix)) (length answer))) (create-matrix (row-count a-matrix) (col-count b-matrix) answer))
                        (else (create-matrix (row-count a-matrix) (col-count b-matrix) answer)))))))

           ; matrix matrix -> matrix
           ; Subtracts two matrixes
           ; given (1 2 (3 3)) (1 2 (2 2)) expect (1 2 (1 1))
           (subtract
            (lambda (a-matrix b-matrix)
              (letrec ((subtract-h
                        (lambda (a-list b-list)
                          (cond ((or (null? a-list) (null? b-list)) (quote ()))
                                (else (cons (- (car a-list) (car b-list)) (subtract-h (cdr a-list) (cdr b-list))))))))
                (let ((answer (subtract-h (as-list a-matrix) (as-list b-matrix))))
                  (cond ((not (= (* (row-count a-matrix) (col-count b-matrix)) (length answer))) (create-matrix (row-count a-matrix) (col-count b-matrix) answer))
                        (else (create-matrix (row-count a-matrix) (col-count b-matrix) answer)))))))

           ; matrix -> matrix
           ; Swaps the row/cols
           ; given (1 2 (3 3)) expect (2 1 (3 3))
           (transpose
            (lambda (a-matrix)
              (create-matrix (col-count a-matrix) (row-count a-matrix) (as-list a-matrix))))
           
           )
    
    ; <pattern> -> <varies>
    ; Grammar for little matrix language
    ; The prototype represents the purely abstract and unconstrained representation of the data-type which may be recognized by matrix lang
    ; prototype: (row col tuple)
    ; The pattern represents the constraints which must be met to be recognized as a matrix by matrix lang
    ; matrix pattern: ((?number? x) (?>=0? x) (?number? y) (?>=0? y) (?list-of-numbers? z) (?size-match? x y z))
    ; 'new number number list    -> matrix
    ; matrix 'row number         -> list
    ; matrix 'col number         -> list
    ; matrix '* matrix           -> matrix
    ; matrix '+ matrix           -> matrix
    ; matrix '- matrix           -> matrix
    ; matrix 'apply function     -> matrix
    ; matrix 'count              -> number
    ; matrix 'transpose          -> matrix
    ; matrix 'is-matrix?         -> boolean
    ; matrix 'parse-matrix       -> matrix
    (match-lambda*
      ((list 'new (? number? x) (? number? y) (? list-of-numbers? z)) (matrix? (list x y z)))
      ((list (? matrix? x) 'row (? number? y)) (get-row x y))
      ((list (? matrix? x) 'col (? number? y)) (get-col x y))
      ((list (? matrix? x) '* (? matrix? y)) (multiply x y))
      ((list (? matrix? x) '+ (? matrix? y)) (add x y))
      ((list (? matrix? x) '- (? matrix? y)) (subtract x y))
      ((list (? matrix? x) 'apply (? procedure? f)) (apply-each f x))
      ((list (? matrix? x) 'count) (count x))
      ((list (? matrix? x) 'transpose) (transpose x))
      ((list x 'is-matrix?) (is-matrix? x))
      ((list x 'parse-matrix) (matrix? x))
      ((list x) x)
      (_ #f)
      )))

; Provide access to this module within other files
(provide matrix-lang)