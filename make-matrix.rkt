#lang racket

; Author: Dustin Gulley
; Class: CSC 525
; Description: Project 4. Represent a matrix and provide accessors/mutators
; Due date: April 26 2017

(define make-matrix
  (lambda (row-count col-count)
    (let ((a-matrix (quote ())))
      
      ; number -> list
      ; Builds a default matrix, this is just used once to set up our matrix
      ; given 3 3, expect (0 0 0 0 0 0 0 0 0)
      ; given 2 3, expect (0 0 0 0 0 0)
      ; given 3 2, expect (0 0 0 0 0 0)
      (letrec ((make-matrix-h (lambda (matrices)
                                (cond ((> matrices 0) (cons 0 (make-matrix-h (- matrices 1))))
                                      (else (quote ()))))))
        (set! a-matrix (make-matrix-h (* row-count col-count))))

      (letrec (

               ; _ -> number
               ; Retrives the number of nodes within the matrix
               ; with matrix 1 3 1, 2 2 3, 3 1 2, expect 9
               (count (lambda () (* row-count col-count)))

               ; number number -> number
               ; Retrieves the number from the node within the matrix
               ; with matrix 1 3 1, 2 2 3, 3 1 2, given 2 2, expect 2
               ; with matrix 1 3 1, 2 2 3, 3 1 2, given 3 1, expect 3
               ; with matrix 1 3 1, 2 2 3, 3 1 2, given 4 1, expect ()
               ; with matrix 1 3 1, 2 2 3, 3 1 2, given 1 4, expect ()
               (get-row-col (lambda (a-row a-col)
                              (letrec ((get-row-col-h (lambda (a-col a-row)
                                                        (cond ((null? a-row) (quote ()))
                                                              ((= a-col 1) (car a-row))
                                                              (else (get-row-col-h (- a-col 1) (cdr a-row)))))))
                                (get-row-col-h a-col (get-row a-row)))))

               ; number number number -> list
               ; Sets the specified row/col to the number.
               ; It will both mutate by setting the exising matrix to have the changed number
               ; and it will also return the mutated matrix
               ; with matrix 1 3 1, 2 2 3, 3 1 2, given 2 2 3, expect (1 3 1 2 3 3 3 1 2)
               ; with matrix 1 3 1, 2 2 3, 3 1 2, given 3 1 2, expect (1 3 1, 2 2 3, 2 1 2)
               ; with matrix 1 3 1, 2 2 3, 3 1 2, given 0 0 2, expect (1 3 1, 2 2 3, 3 1 2)
               ; with matrix 1 3 1, 2 2 3, 3 1 2, given 4 4 4, expect (1 3 1, 2 2 3, 3 1 2)
               (set-row-col! (lambda (a-row a-col a-node)
                               (let/cc skip
                                 (letrec ((set-row-col-h! (lambda (a-row a-col matrix)
                                                            (cond ((null? matrix) (skip a-matrix))
                                                                  ((and (eq? a-row 1) (eq? a-col 1)) (cons a-node (cdr matrix)))
                                                                  ((eq? a-col 1) (cons (car matrix) (set-row-col-h! (- a-row 1) col-count (cdr matrix))))
                                                                  (else  (cons (car matrix) (set-row-col-h! a-row (- a-col 1) (cdr matrix))))))))
                                   (let () (set! a-matrix (set-row-col-h! a-row a-col a-matrix))) a-matrix))))

               ; number -> list
               ; Returns the specified column from the matrix
               ; with matrix 1 3 1, 2 2 3, 3 1 2, given 2, expect (3 2 1)
               ; with matrix 1 2 3, 3 2 1, 1 3 2, given 3, expect (3 1 2)
               ; with matrix 1 3 1, 2 2 3, 3 1 2, given -1, expect ()
               ; with matrix 1 3 1, 2 2 3, 3 1 2, given 0, expect ()
               ; with matrix 1 3 1, 2 2 3, 3 1 2, given 4, expect ()
               (get-col (lambda (a-col)
                          ; Helper function so we don't recur on a-col
                          (letrec ((get-col-h (lambda (rows cols matrix)
                                                (cond ((> cols col-count) (get-col-h (+ rows 1) 1 matrix))
                                                      ((or (> rows row-count) (null? matrix)) (quote ()))
                                                      ((= cols a-col) (cons (car matrix) (get-col-h rows (+ cols 1) (cdr matrix))))
                                                      (else (get-col-h rows (+ cols 1) (cdr matrix)))))))
                            (get-col-h 1 1 a-matrix))))

               ; number -> list
               ; Returns the specified row from the matrix
               ; with matrix 1 3 1, 2 2 3, 3 1 2, given 2, expect (2 2 3)
               ; with matrix 1 2 3, 3 2 1, 1 3 2, given 3, expect (1 3 2)
               ; with matrix 1 3 1, 2 2 3, 3 1 2, given -1, expect ()
               ; with matrix 1 3 1, 2 2 3, 3 1 2, given 0, expect ()
               ; with matrix 1 3 1, 2 2 3, 3 1 2, given 4, expect ()
               (get-row (lambda (a-find)
                          ;Helper function so we don't keep recurring on a-find
                          (letrec ((get-row-h (lambda (rows cols matrix)
                                                (cond ((> cols col-count) (get-row-h (+ rows 1) 1 matrix))
                                                      ((or (> rows a-find) (null? matrix)) (quote ()))
                                                      ((= rows a-find) (cons (car matrix) (get-row-h rows (+ cols 1) (cdr matrix))))
                                                      (else (get-row-h rows (+ cols 1) (cdr matrix)))))))
                            (get-row-h 1 1 a-matrix))))

               ; list -> list
               ; Sets a completely new matrix
               ; note that the new matrix should fit the requirements of the existing col/size limit
               ; there is no checking to ensure the col/size limit is the same.  If an incorrectly sized
               ; matrix is put in, it will cause weird errors.
               ; This will return the new matrix as a list as well as set the current matrix to it
               ; given (1 3 1 2 2 3 3 1 2), expect (1 3 1 2 2 3 3 1 2)
               (set-matrix! (lambda (matrix)
                              (let () (set! a-matrix matrix)) a-matrix))
               
               ; list -> list
               ; Multiplies two matrixs.  It does no mutation but instead returns a list of the the values of the multiplied matrixes
               ; Note the both matrixes are expected to have matching the row count match the col count of the other,
               ; If they don't fit this format.. problems will happen
               ; given (1 3 1 2 2 3 3 1 2) * (1 2 3 3 2 1 1 3 2), expect (11 11 8 11 17 14 8 14 14)
               (multiply (lambda (b-matrix)
                           
                           ;Helper function for multiplying two lists in a row/col way
                           (letrec ((multiple-add-lists (lambda (a-list b-list)
                                                          (cond ((or (null? a-list) (null? b-list)) 0)
                                                                (else (+ (* (car a-list) (car b-list)) (multiple-add-lists (cdr a-list) (cdr b-list)))))))
                                    ;Helper function to perform the multiplication and not have to recur on b-matrix               
                                    (multiply-h (lambda (row-n col-n)
                                                  (let ((row (get-row row-n))
                                                        (col ((b-matrix 'get-col) col-n)))
                                                    (cond ((> row-n row-count) (quote ()))
                                                          ((= col-n row-count) (cons (multiple-add-lists row col) (multiply-h (+ 1 row-n) 1)))
                                                          (else (cons (multiple-add-lists row col) (multiply-h row-n (+ col-n 1)))))))))
                             (multiply-h 1 1))))
               )

        ; atom -> <varies>
        ; incorporates closure based message passing
        (lambda (message)
          (cond ((eq? message 'get-matrix) a-matrix)
                ((eq? message 'get-row-col) (lambda (a-row a-col) (get-row-col a-row a-col)))
                ((eq? message 'set-row-col!) (lambda (a-row a-col a-node) (set-row-col! a-row a-col a-node)))
                ((eq? message 'count) (count))
                ((eq? message '*) (lambda (a-matrix) (multiply a-matrix))) ;todo
                ((eq? message 'set!) (lambda (a-matrix) (set-matrix! a-matrix)))
                ((eq? message 'get-col) (lambda (a-col) (get-col a-col)))
                ((eq? message 'get-row) (lambda (a-row) (get-row a-row)))
                ((eq? message 'get-row-count) row-count)
                ((eq? message 'get-col-count) col-count)
                (else (quote ()))))))))

; Provide access to this module within other files
(provide make-matrix)