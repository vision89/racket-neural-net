#lang racket
(require math/base)
(require "make-matrix.rkt")

; Author: Dustin Gulley
; Class: CSC 525
; Description: Project 4.  Initialize and return a neural network.
; Due date: April 26 2017

(define neural-net-lang
  (letrec (

           (matrix? (lambda (x)
                      (cond ((eq? ':matrix (get-type x)) x)
                            (else (quote ())))))

           (neural-network? (lambda (x)
                              (cond ((eq? ':neural-network (get-type x)) x)
                                    (else (quote ())))))

           (get-type
            (lambda (x)
              (car x)))

           (get-input-node-count
            (lambda (x)
              (cadr x)))

           (get-hidden-node-count
            (lambda (x)
              (caddr x)))

           (get-output-node-count
            (lambda (x)
              (cadddr x)))

           (get-learning-rate
            (lambda (x)
              (car (cddddr x))))

           (get-weighted-hidden-input
            (lambda (x)
              (car (cdr (cddddr x)))))

           (get-weighted-hidden-output
            (lambda (x)
              (car (cdr (cdr (cddddr x))))))
           
           (create-neural-network
            (lambda (w x y z)
              (list ':neural-network w x y z
                    (matrix-lang 'new x x (list-of-random-vals (* x x) 100 100 1/2))
                    (matrix-lang 'new y y (list-of-random-vals (* y y) 100 100 1/2)))))
           
           ; number -> number
           ; Sigmoid function
           (activation (lambda (x)
                         (/ 1 (+ 1 (expt euler.0 (* x -1))))))

           ; matrix matrix -> matrix
           ; Train the neural network
           (train (lambda (a-neural-network input-m target-m)
                    (matrix-lang (matrix-lang (get-weighted-hidden-output a-neural-network) '*
                                              (matrix-lang (matrix-lang (get-weighted-hidden-input a-neural-network) '*
                                                                        input-m) 'apply activation)) 'apply activation)))

           ; matrix -> matrix
           ; Query the neural network
           (query (lambda (a-neural-network input-m)
                    (matrix-lang (matrix-lang (get-weighted-hidden-output a-neural-network) '*
                                              (matrix-lang (matrix-lang (get-weighted-hidden-input a-neural-network) '*
                                                                        input-m) 'apply activation)) 'apply activation)))
           )

    ; <pattern> -> <varies>
    ; Grammar for little neural network language
    ; prototype (type input-node-count hidden-node-count output-node-count learning-rate weighted-hidden-output weighted-hidden-input)
    ; 'new number number number number    -> neural-network
    ; 'new number number                  -> neural-network
    ; neural-network 'train matrix matrix -> matrix
    ; neural-network 'quert matrix        -> matrix
    (match-lambda*
      ((list 'new (? number? w) (? number? x) (? number? y) (? number? z)) (create-neural-network w x y z))
      ((list 'new (? number? x) (? number? y)) (create-neural-network x x x y))
      ((list (? neural-network? x) 'train (? matrix? y) (? matrix? z)) (train x y z))
      ((list (? neural-network? x) 'query (? matrix? y)) (query x y))
      (x x)
      (_ #f))))

; Provide access to this module within other files
(provide neural-net-lang)