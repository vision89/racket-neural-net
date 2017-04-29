#lang racket
(require math/base)
(require "make-matrix.rkt")

; Author: Dustin Gulley
; Class: CSC 525
; Description: Project 4.  Initialize and return a neural network.
; Due date: April 26 2017

(define neural-net-lang
  (letrec (

           (replace-nth
            (lambda (a-list x n)
              (letrec ((replace-nth-h (lambda (a-list n)
                                        (cond ((null? a-list) (quote ()))
                                              ((= n 1) (cons x (cdr a-list)))
                                              (else (cons (car a-list) (replace-nth-h (cdr a-list) (- n 1))))))))
                (replace-nth-h a-list n))))

           (matrix?
            (lambda (x)
              (matrix-lang x 'parse-matrix)))

           (meets-neural-network-constraints?
            (lambda (x)
              (cond ((and (eq? (length x) 3) (> (car (cdr (cdr x))) 0)) x)
                    (else (quote ())))))

           (neural-network?
            (lambda (x)
              (cond ((and (is-neural-network-prototype? x) (meets-neural-network-constraints? x)) x)
                    (else (quote ())))))

           (get-learning-rate
            (lambda (x)
              (car (cdr (cdr x)))))

           (get-weighted-hidden-input
            (lambda (x)
              (car x)))

           (get-weighted-hidden-output
            (lambda (x)
              (car (cdr x))))
           
           (create-neural-network
            (lambda (whi who lr)
              (list whi who lr)))
           
           ; number -> number
           ; Sigmoid function
           (activation (lambda (x)
                         (/ 1 (+ 1 (expt euler.0 (* x -1))))))

           (activation-prime (lambda (x)
           (* (/ 1
                 (+ 1 (expt euler.0 (* x -1))))
              (/ (expt (* -1 euler.0) (* -1 x)) (+ 1 (expt euler.0 (* x -1)))))))

           (apply-to-list
            (lambda (f l)
              (letrec ((apply-to-list-h
                        (lambda (l)         
                          (cond ((null? l) (quote ()))
                                (else (cons (f (car l) (apply-to-list-h (cdr l)))))))))
                (apply-to-list-h l))))

           (calculate-error
            (lambda (a-list b-list)
              (cond ((or (null? a-list) (null? b-list)) (quote ()))
                    (else (cons (* (- (car a-list) (car b-list)) (activation-prime (car b-list))) (calculate-error (cdr a-list) (cdr b-list)))))))

           (calculate-hidden-error
            (lambda (a-list b-list)
              (cond ((or (null? a-list) (null? b-list)) 0)
                    (else (+ (* (car a-list) (car b-list)) (calculate-hidden-error (cdr a-list) (cdr b-list)))))))
              
                               
           ; matrix matrix -> matrix
           ; Train the neural network
           (train
            (lambda (a-neural-network input output)
              (letrec ((train-h
                        (lambda (a-neural-network input-list output-list)
                          (cond ((or (null? input-list) (null? output-list)) a-neural-network)
                                (else
                                 (let* (
                                        (l0 (car input-list))
                                        (y (car output-list))
                                        (learning-rate (get-learning-rate a-neural-network))
                                        (syn0 (get-weighted-hidden-input a-neural-network))
                                        (syn1 (get-weighted-hidden-output a-neural-network))

                                        ;FF
                                        (l1 (matrix-lang (matrix-lang l0 '* syn0) 'apply (lambda (x) (activation x))))
                                        (l2 (matrix-lang (matrix-lang l1 '* syn1) 'apply (lambda (x) (activation x))))

                                        ;Back Propogation
                                        (l2_error (matrix-lang y '- l2))
                                        (l2_delta (matrix-lang l2_error '* (matrix-lang l2 'apply (lambda (x) (activation-prime x)))))
                                        (l1_error (matrix-lang l2_delta '* (matrix-lang syn1 'transpose)))
                                        (l1_delta (matrix-lang l1_error '* (matrix-lang l1 'apply (lambda (x) (activation-prime x)))))
                                        (new-weighted-hidden-output (matrix-lang syn1 '+ 
                                                                                 (matrix-lang (matrix-lang l1 'transpose) '* l2_delta)))
                                        (new-weighted-hidden-input (matrix-lang syn0 '+
                                                                                (matrix-lang (matrix-lang l0 'transpose) '* l1_delta)))

                                        )
                                   (train-h (list new-weighted-hidden-input new-weighted-hidden-output 1) (cdr input-list) (cdr output-list))))))))
                (train-h a-neural-network input output))))

           ; matrix -> matrix
           ; Query the neural network
           (query
            (lambda (a-neural-network input-layer)
              (let* (
                     (weighted-hidden-input (get-weighted-hidden-input a-neural-network))
                     (weighted-hidden-output (get-weighted-hidden-output a-neural-network))
                     (hidden-inputs (matrix-lang input-layer '* weighted-hidden-input))
                     (hidden-outputs (matrix-lang hidden-inputs 'apply (lambda (x) (activation x))))
                     (final-inputs (matrix-lang hidden-outputs '* weighted-hidden-output))
                     (final-outputs (matrix-lang final-inputs 'apply (lambda (x) (activation x))))
                     )
                    
                final-outputs)))

           (is-neural-network-prototype?
            (lambda (x)
              (cond ((and (eq? (length x) 3) (matrix? (car x)) (matrix? (car (cdr x))) (number? (car (cdr (cdr x))))) #t)
                    (else #f))))
           
           )

    ; <pattern> -> <varies>
    ; Grammar for little neural network language
    ; prototype (number number number number matrix matrix))
    ; 'new number number number number    -> neural-network
    ; neural-network 'train matrix matrix -> matrix
    ; neural-network 'quert matrix        -> matrix
    (match-lambda*
                  
      ((list 'new (? matrix? x) (? matrix? y) (? number? z)) (create-neural-network x y z))
      ((list (? neural-network? w) 'train (? list? x) (? list? y)) (train w x y))
      ((list (? neural-network? x) 'query (? matrix? y)) (query x y))
      (x x)
      (_ #f))))

; Provide access to this module within other files
(provide neural-net-lang)