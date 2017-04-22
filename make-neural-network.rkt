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
              (println 'neural-network-checking-matrix)
              (matrix-lang x 'parse-matrix)))

           (meets-neural-network-constraints?
            (lambda (x)
              (println 'checking-neural-network-constraints)
              (cond ((and (eq? (length x) 3) (> (car (cdr (cdr x))) 0)) x)
                    (else (quote ())))))

           (neural-network?
            (lambda (x)
              (cond ((and (is-neural-network-prototype? x) (meets-neural-network-constraints? x)) x)
                    (else (quote ())))))

           (get-learning-rate
            (lambda (x)
              (println 'getting-learning-rate:)(println x)
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

           ; matrix matrix -> matrix
           ; Train the neural network
           (train
            (lambda (a-neural-network input-m target-m)

              (let* (
                     (hidden-outputs (matrix-lang (matrix-lang (get-weighted-hidden-input a-neural-network) '* input-m) 'apply activation))
                     (final-outputs (matrix-lang (matrix-lang (get-weighted-hidden-output a-neural-network) '* hidden-outputs) 'apply activation))
                     (output-errors (matrix-lang (get-weighted-hidden-input a-neural-network) '* (matrix-lang target-m '- final-outputs)))
                     (hidden-errors (matrix-lang (get-weighted-hidden-output a-neural-network) '* output-errors))
                     ;who += learning-rate * (output-errors * final-outputs * (1.0 - final-outputs)) * transpose(hidden-outputs)
                     (new-who (matrix-lang (get-weighted-hidden-output a-neural-network) '+
                                           (matrix-lang
                                            (matrix-lang (matrix-lang (matrix-lang output-errors '* final-outputs) '* (matrix-lang final-outputs 'apply (lambda (x) (- 1.0 x))))
                                             'apply (lambda (x) (* x (get-learning-rate a-neural-network)))) '* (matrix-lang hidden-outputs 'transpose))))
                     ;whi += learning-rate * (hidden-errors * hidden-outputs * (1.0 - hidden-outputs)) * transpose(input-m)
                     (new-wih (matrix-lang (get-weighted-hidden-input a-neural-network) '+
                                           (matrix-lang
                                            (matrix-lang (matrix-lang (matrix-lang hidden-errors '* hidden-outputs) '* (matrix-lang hidden-outputs 'apply (lambda (x) (- 1.0 x))))
                                             'apply (lambda (x) (* x (get-learning-rate a-neural-network)))) '* (matrix-lang input-m 'transpose))))
                     )
                (println 'done)
                (cons (car a-neural-network) (replace-nth (replace-nth a-neural-network new-wih 1) new-who 2)))))

           ; matrix -> matrix
           ; Query the neural network
           (query
            (lambda (a-neural-network input-m)
              (matrix-lang (matrix-lang (get-weighted-hidden-output a-neural-network) '*
                                        (matrix-lang (matrix-lang (get-weighted-hidden-input a-neural-network) '*
                                                                  input-m) 'apply activation)) 'apply activation)))

           (is-neural-network-prototype?
            (lambda (x)
              (println 'checking-is-neural-network-prototype)
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
      ((list 'new (? matrix? x) (? matrix? y) (? number? z)) (println 'in-new)(create-neural-network x y z))
      ((list (? neural-network? x) 'train (? matrix? y) (? matrix? z)) (println 'in-train)(train x y z))
      ((list (? neural-network? x) 'query (? matrix? y)) (println 'in-query)(query x y))
      (x (println 'in-identity)x)
      (_ #f))))

; Provide access to this module within other files
(provide neural-net-lang)