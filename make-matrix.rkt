#lang racket

; Author: Dustin Gulley
; Class: CSC 525
; Description: Project 4. Represent a matrix and provide accessors/mutators
; Due date: April 26 2017

(define make-matrix
  (lambda (row-count col-count)
    (let ((a-matrix (quote ())))
      (letrec ((make-matrix-h (lambda (rows cols)
                                (cond ((and (> rows 0) (> cols 0)) (cons (quote ()) (make-matrix-h rows (- cols 1))))
                                      ((and (> rows 0) (= cols 0)) (make-matrix-h (- rows 1) col-count))
                                      (else (quote ()))))))
        (let () (set! a-matrix (make-matrix-h row-count col-count)))
        (lambda (message)
          (cond ((eq? message 'get-matrix) a-matrix)
                (else (quote ()))))))))

; Provide access to this module within other files
(provide make-matrix)