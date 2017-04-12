#lang racket
 (require math/base)

; Author: Dustin Gulley
; Class: CSC 525
; Description: Project 4.  Initialize and return a neural network.
; Due date: April 26 2017

; positive-int positive-int positive-int positive-float -> function
; Initializes the neural network and returns a closure for manipulating the neural netowrk
(define make-neural-network
  (lambda (input-node-count hidden-node-count output-node-count learning-rate)
    
    ; atom -> <varies>
    ; Message passing logic for the closure structure
    ; given 'message, expect the identity function (todo: replace identity function)
    ; given 'query, expect the identity function (todo: replace identity function)
    (lambda (message)

      (cond
        ; Train the neural network, I'm just returning identity while I develop this
        ((eq? message 'train) (lambda (a-var) a-var))

        ; Query the neural network, I'm just returning identity while I develop this
        ((eq? message 'query) (lambda (a-var) a-var))

        ; Default to null
        (else (quote ())))
      )))

(define sigmoid-function (lambda (x)
                           (/ 1 (+ 1 (exp euler.0 (* x -1))))))

; Provide access to this module within other files
(provide make-neural-network)