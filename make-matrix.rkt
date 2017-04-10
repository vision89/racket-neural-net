#lang racket

; Author: Dustin Gulley
; Class: CSC 525
; Description: Project 4. Represent a matrix and provide accessors/mutators
; Due date: April 26 2017

(define make-matrix
  (lambda (row-count col-count)
    (let ((a-matrix (quote ())))
      (letrec ((make-matrix-h (lambda (matrices)
                                (cond ((> matrices 0) (cons (quote ()) (make-matrix-h (- matrices 1))))
                                      (else (quote ()))))))
        (let () (set! a-matrix (make-matrix-h (* row-count col-count))))
        (letrec ((count (lambda ()
                          (letrec ((count-h (lambda (count a-matrix)
                                              (cond ((null? a-matrix) count)
                                                    (else (count-h (+ count 1) (cdr a-matrix)))))))
                            (count-h 0 a-matrix))))
                 (get-row-col (lambda (a-row a-col)
                                (letrec ((get-row-col-h (lambda (matrices a-matrix)
                                                          (cond ((> matrices 0) (get-row-col-h (- matrices 1) (cdr a-matrix)))
                                                                (else (car a-matrix))))))
                                  (get-row-col-h (* a-row a-col)))))
                 (set-row-col! (lambda a-row a-col a-node)
                               (letrec ((set-row-col-h! (lambda (matrices matrix)
                                                          (cond ((= matrices 0) (cons a-node (set-row-col-h! matrices)))
                                                                ((not (null? matrix)) (cons (car matrix) (set-row-col-h! (- matrices 1))))
                                                                (else matrix)))))
                                 (set! a-matrix (set-row-col-h! (* a-row a-col) a-matrix))))
                 
                 )
        (lambda (message)
          (cond ((eq? message 'get-matrix) a-matrix)
                ((eq? message 'get-row-col) (lambda (a-row a-col) (get-row-col a-row a-col)))
                ((eq? message 'set-row-col!) (lambda (a-row a-col a-node) (set-row-col! a-row a-col a-node)))
                ((eq? message 'count) (count))
                ((eq? message '*) (lambda (a-matrix) (multiply a-matrix))) ;todo
                ((eq? message 'set!) (lambda (a-matrix) (set-matrix! a-matrix))) ;todo
                ((eq? message 'get-col) (lambda (a-col) (get-col a-col))) ;todo
                ((eq? message 'get-row) (lambda (a-row) (get-row a-row))) ;todo
                (else (quote ())))))))))

; Provide access to this module within other files
(provide make-matrix)