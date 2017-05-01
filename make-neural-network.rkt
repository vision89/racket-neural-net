#lang racket
; Author: Dustin Gulley
; Class: CSC 525
; Description: Project 4.  Initialize and return a neural network.
; Due date: April 26 2017

(require math/base)
(require "make-matrix.rkt")

; A small language for using a neural network
(define neural-net-lang
  (letrec (

           ; list -> list
           ; Checks if the parameter meets the constraints of a neural network
           ; if it does it returns it, if not it returns null
           ; given ((1 2 (2 2)) (1 2 (2 2)) 3) expect ((1 2 (2 2)) (1 2 (2 2)) 3)
           ; given (1 2 3 4 5) expect null
           (meets-neural-network-constraints?
            (lambda (x)
              (cond ((and (eq? (length x) 3) (> (car (cdr (cdr x))) 0)) x)
                    (else (quote ())))))

           ; list -> list
           ; Checks if the parameter meets the prototype of a neural network
           ; if it does it returns it, if not it returns null
           ; given ((1 2 (2 2)) (1 2 (2 2)) 3) expect ((1 2 (2 2)) (1 2 (2 2)) 3)
           ; given (1 2 3 4 5) expect null
           (neural-network?
            (lambda (x)
              (cond ((and (is-neural-network-prototype? x) (meets-neural-network-constraints? x)) x)
                    (else (quote ())))))

           ; neural-network -> number
           ; Returns the learning rate from a neural network
           ; given ((1 2 (2 2)) (1 2 (2 2)) 3) expect 3
           (get-learning-rate
            (lambda (x)
              (car (cdr (cdr x)))))

           ; neural-network -> matrix
           ; Returns the weighted hidden input of a neural network
           ; given ((1 2 (2 2)) (1 2 (3 3)) 3) expect (1 2 (2 2))
           (get-weighted-hidden-input
            (lambda (x)
              (car x)))

           ; neural-network -> matrix
           ; Returns the weighted hidden output of a neural network
           ; given ((1 2 (2 2)) (1 2 (3 3)) 3) expect (1 2 (3 3))
           (get-weighted-hidden-output
            (lambda (x)
              (car (cdr x))))

           ; matrix matrix number -> neural-network
           ; Creates a neural network from the given parameters
           ; given (1 2 (2 2)) (1 2 (3 3)) 3 expect ((1 2 (2 2)) (1 2 (3 3)) 3)
           (create-neural-network
            (lambda (whi who lr)
              (list whi who lr)))
           
           ; number -> number
           ; Sigmoid function
           (activation (lambda (x)
                         (/ 1 (+ 1 (expt euler.0 (* x -1))))))

           ; number -> number
           ; Sigmoid function derivitive
           (activation-prime (lambda (x)
           (* (/ 1
                 (+ 1 (expt euler.0 (* x -1))))
              (/ (expt (* -1 euler.0) (* -1 x)) (+ 1 (expt euler.0 (* x -1)))))))
                               
           ; matrix matrix -> matrix
           ; Train the neural network
           (train
            (lambda (a-neural-network input output)
              (cond ((or (null? input) (null? output)) a-neural-network)
                    (else
                     (let* (
                            ;Organize the values
                            (input-layer (car input))
                            (output-layer (car output))
                            (learning-rate (get-learning-rate a-neural-network))
                            (whi (get-weighted-hidden-input a-neural-network))
                            (who (get-weighted-hidden-output a-neural-network))

                            ;FF
                            (final-inputs (matrix-lang (matrix-lang input-layer '* whi) 'apply (lambda (x) (activation x))))
                            (final-outputs (matrix-lang (matrix-lang final-inputs '* who) 'apply (lambda (x) (activation x))))

                            ;Back Propogation
                            (who_error (matrix-lang output-layer '- final-outputs))
                            (who_delta (matrix-lang who_error '* (matrix-lang final-outputs 'apply (lambda (x) (activation-prime x)))))
                            (whi_error (matrix-lang who_delta '* (matrix-lang who 'transpose)))
                            (whi_delta (matrix-lang whi_error '* (matrix-lang final-inputs 'apply (lambda (x) (activation-prime x)))))
                            (new-weighted-hidden-output (matrix-lang who '+ 
                                                                     (matrix-lang (matrix-lang (matrix-lang (matrix-lang final-inputs 'transpose) '* who_delta)) 'apply (lambda (x) (* x learning-rate)))))
                            (new-weighted-hidden-input (matrix-lang whi '+
                                                                    (matrix-lang (matrix-lang (matrix-lang input-layer 'transpose) '* whi_delta) 'apply (lambda (x) (* x learning-rate)))))

                            )
                       (train (list new-weighted-hidden-input new-weighted-hidden-output learning-rate) (cdr input) (cdr output)))))))

           ; matrix -> matrix
           ; Query the neural network
           (query
            (lambda (a-neural-network input-layer)
              (let* (

                     ;Organize values
                     (learning-rate (get-learning-rate a-neural-network))
                     (whi (get-weighted-hidden-input a-neural-network))
                     (who (get-weighted-hidden-output a-neural-network))

                     ;FF
                     (final-inputs (matrix-lang (matrix-lang input-layer '* whi) 'apply (lambda (x) (activation x))))
                     (final-outputs (matrix-lang (matrix-lang final-inputs '* who) 'apply (lambda (x) (activation x))))
                     )
                    
                final-outputs)))

           ; s-expr -> bool
           ; Checks if the parameter is a nerual-network, if so it returns true, if not it returns false
           ; given ((1 2 (2 2)) (1 2 (2 2)) 3) expect true
           ; given () expect false
           (is-neural-network-prototype?
            (lambda (x)
              (cond ((and (eq? (length x) 3) (matrix-lang (car x) 'parse-matrix) (matrix-lang (car (cdr x)) 'parse-matrix) (number? (car (cdr (cdr x))))) #t)
                    (else #f))))
           
           )

    ; <pattern> -> <varies>
    ; Grammar for little neural network language
    ; prototype (matrix matrix number)
    ; 'new matrix matrix number           -> neural-network
    ; neural-network 'train matrix matrix -> matrix
    ; neural-network 'query matrix        -> matrix
    ; atom                                -> atom
    ; _                                   -> false
    (match-lambda*   
      ((list 'new x y z) (create-neural-network x y z))
      ((list (? neural-network? w) 'train x y) (train w x y))
      ((list (? neural-network? x) 'query y) (query x y))
      (x x)
      (_ #f))))

; Provide access to this module within other files
(provide neural-net-lang)