#lang racket

; Author: Dustin Gulley
; Class: CSC 525
; Description: Project 4. Represent a matrix and provide accessors/mutators
; Due date: April 26 2017

(define matrix-lang
  (letrec (


           ; *** Parsing Logic ***

           (is-prototype?
            (lambda x
              (cond ((and (eq? (length x) 3) (number? (car x)) (number? (cadr x))) x)
                    (else (quote ())))))

           (num>=0?
            (lambda (x)
              (cond ((and (number? x) (>= x 0) x))
                    (else (quote ())))))

           (tuple?
            (lambda (a-tup)
              (let/cc skip
                (cond ((null? a-tup) (quote ()))
                      ((number? (car a-tup)) (cons (car a-tup) (tuple? (cdr a-tup))))
                      (else (skip (quote ())))))))

           (matrix-match?
            (lambda x
              (cond ((and (eq? (length x) 3) (num>=0? (car x)) (num>=0? (cadr x))) x)
                    (else (quote ())))))
           
           (matrix?
            (lambda (x)
              (cond ((and (eq? (length x) 3) (is-prototype? (car x) (cadr x) (caddr x)) (matrix-match? (car x) (cadr x) (caddr x))) x)
                    (else (quote ())))))

           (is-matrix?
            (lambda x
              (cond ((matrix? x) #t)
                    (else #f))))
 

           ; *** matrix logic ****

           (empty-matrix
            (lambda ()
              (list (list 0 0 '(quote ())))))

           (create-matrix
            (match-lambda*
              ((list (? number? x) (? number? y) (? list? z)) (list x y z))
              ((list (? number? x) (? list? y)) (list x x y))
              (_ (empty-matrix))))

           (row-count
            (lambda (x)
              (car x)))

           (col-count
            (lambda (x)
              (cadr x)))

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

           (add
            (lambda (a-matrix b-matrix)
              (letrec ((add-h
                        (lambda (a-list b-list)
                          (cond ((or (null? a-list) (null? b-list)) (quote ()))
                                (else (cons (+ (car a-list) (car b-list)) (add-h (cdr a-list) (cdr b-list))))))))
                (let ((answer (add-h (as-list a-matrix) (as-list b-matrix))))
                  (cond ((not (= (* (row-count a-matrix) (col-count b-matrix)) (length answer))) (println 'messedup-add!)(println a-matrix)(println b-matrix)(println answer)(create-matrix (row-count a-matrix) (col-count b-matrix) answer))
                        (else (create-matrix (row-count a-matrix) (col-count b-matrix) answer)))))))

           (subtract
            (lambda (a-matrix b-matrix)
              (letrec ((subtract-h
                        (lambda (a-list b-list)
                          (cond ((or (null? a-list) (null? b-list)) (quote ()))
                                (else (cons (- (car a-list) (car b-list)) (subtract-h (cdr a-list) (cdr b-list))))))))
                (let ((answer (subtract-h (as-list a-matrix) (as-list b-matrix))))
                  (cond ((not (= (* (row-count a-matrix) (col-count b-matrix)) (length answer))) (println 'messedup-subtract!)(println a-matrix)(println b-matrix)(println answer)(create-matrix (row-count a-matrix) (col-count b-matrix) answer))
                        (else (create-matrix (row-count a-matrix) (col-count b-matrix) answer)))))))

           (transpose
            (lambda (a-matrix)
              (create-matrix (col-count a-matrix) (row-count a-matrix) (as-list a-matrix))))

           (dot-product
            (lambda (a-matrix b-matrix)
              (letrec ((dot-product-h
                        (lambda (list-a list-b)
                          (cond ((or (null? list-a) (null? list-b)) 0)
                                (else (+ (* (car list-a) (car list-b)) (dot-product-h (cdr list-a) (cdr list-b))))))))
                (dot-product-h (as-list a-matrix) (as-list b-matrix)))))

           (sum (lambda (a-matrix b-matrix)
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
                             (sum-h
                              (lambda (row-n col-n)
                                (let ((row (get-row a-matrix row-n))
                                      (col (get-col b-matrix col-n)))
                                  (cond ((> row-n total-rows) 0)
                                        ((> col-n total-cols) (sum-h (+ 1 row-n) 1))
                                        (else (+ (multiple-add-lists row col) (sum-h row-n (+ col-n 1)))))))))
                      (sum-h 1 1)))))

           (sum-rows-to-list
            (lambda (m)
              (let ((total-rows (row-count m)))
                    (letrec
                        ((sum-list
                          (lambda (l)
                            (cond ((null? l) 0)
                                  (else (+ (car l) (sum-list (cdr l)))))))
               
                         (sum-rows-to-list-h
                          (lambda (row-num)
                            (cond ((> row-num total-rows) (quote ()))
                                  (else (cons (sum-list (get-row m row-num)) (sum-rows-to-list-h (+ row-num 1))))))))
                      (sum-rows-to-list-h 1)))))
           

           )
    
    ; <pattern> -> <varies>
    ; Grammar for little matrix language
    ; The prototype represents the purely abstract and unconstrained representation of the data-type which may be recognized by matrix lang
    ; prototype: (row col tuple)
    ; The pattern represents the constraints which must be met to be recognized as a matrix by matrix lang
    ; matrix pattern: ((?number? x) (?>=0? x) (?number? y) (?>=0? y) (?tuple? z) (?size-match? x y z))
    ; 'new number number list    -> matrix
    ; 'new number list           -> matrix
    ; matrix 'row number         -> list
    ; matrix 'col number         -> list
    ; matrix '* matrix           -> matrix
    ; matrix '+ matrix           -> matrix
    ; matrix '+ matrix           -> matrix
    ; matrix 'apply function     -> matrix
    ; matrix 'count              -> number
    ; matrix 'transpose          -> matrix
    ; var 'is-matrix?            -> boolean    
    (match-lambda*
      ((list 'new (? number? x) (? number? y) (? tuple? z)) (matrix? (list x y z)))
      ((list 'new (? number? x) (? tuple? y)) (matrix? (list x x y)))
      ((list (? matrix? x) 'row (? number? y)) (get-row x y))
      ((list (? matrix? x) 'col (? number? y)) (get-col x y))
      ((list (? matrix? x) '* (? matrix? y)) (multiply x y))
      ((list (? matrix? x) 'dot (? matrix? y)) (dot-product x y))
      ((list (? matrix? x) '+ (? matrix? y)) (add x y))
      ((list (? matrix? x) '- (? matrix? y)) (subtract x y))
      ((list (? matrix? x) 'apply (? procedure? f)) (apply-each f x))
      ((list (? matrix? x) 'count) (count x))
      ((list (? matrix? x) 'transpose) (transpose x))
      ((list (? matrix? x) 'sum (? matrix? y)) (sum x y))
      ((list x 'is-matrix?) (is-matrix? x))
      ((list (? is-matrix? x) 'sum-rows-to-list) (sum-rows-to-list x))
      ((list x 'parse-matrix) (matrix? x))
      ((list x) x)
      (_ #f)
      )))

; Provide access to this module within other files
(provide matrix-lang)