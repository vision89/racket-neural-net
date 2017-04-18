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

           (num>=0?
            (lambda (x)
              (cond ((and (number? x) (>= x 0) x))
                    (else (quote ())))))

           (meets-neural-network-constraints?
            (lambda (x)
              (cond ((and (eq? (length x) 6) (num>=0? (car x)) (num>=0? (car (cdr x))) (num>=0? (car (cddr x))) (num>=0? (car (cdddr x)))) x)
                    (else (quote ())))))

           (neural-network?
            (lambda (x)
              (cond ((and (is-neural-network-prototype? x) (meets-neural-network-constraints? x)) x)
                    (else (quote ())))))

           (get-input-node-count
            (lambda (x)
              (car x)))

           (set-input-node-count
            (lambda (a-neural-network node-count)
              (cons (car a-neural-network) (cons node-count (cdr a-neural-network)))))

           (get-hidden-node-count
            (lambda (x)
              (cadr x)))

           (get-output-node-count
            (lambda (x)
              (caddr x)))

           (get-learning-rate
            (lambda (x)
              (car (cdddr x))))

           (get-weighted-hidden-input
            (lambda (x)
              (car (cddddr x))))

           (get-weighted-hidden-output
            (lambda (x)
              (car (cdr (cddddr x)))))
           
           (create-neural-network
            (lambda (w x y z)
              (list w x y z
                    (matrix-lang 'new x x (list-of-random-vals (* x x) 100 100 1/2))
                    (matrix-lang 'new y y (list-of-random-vals (* y y) 100 100 1/2)))))
           
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
                     (output-errors (matrix-lang (get-weighted-hidden-input a-neural-network) '* (target-m '- final-outputs)))
                     (hidden-errors (matrix-lang (get-weighted-hidden-output a-neural-network) '* output-errors))
                     ;who += learning-rate * (output-errors * final-outputs * (1.0 - final-outputs)) * transpose(hidden-outputs)
                     (new-who (matrix-lang (get-weighted-hidden-output a-neural-network) '+
                                           (matrix-lang
                                            (((matrix-lang output-errors '* final-outputs) '* (matrix-lang final-outputs 'apply (lambda (x) (- 1.0 x))))
                                             'apply (lambda (x) (* x (get-learning-rate a-neural-network)))) '* (matrix-lang hidden-outputs 'transpose))))
                     ;whi += learning-rate * (hidden-errors * hidden-outputs * (1.0 - hidden-outputs)) * transpose(input-m)
                     (new-wih (matrix-lang (get-weighted-hidden-input a-neural-network) '+
                                           (matrix-lang
                                            (((matrix-lang hidden-errors '* hidden-outputs) '* (matrix-lang hidden-outputs 'apply (lambda (x) (- 1.0 x))))
                                             'apply (lambda (x) (* x (get-learning-rate a-neural-network)))) '* (matrix-lang input-m 'transpose))))
                     )
                (cons (car a-neural-network) (replace-nth (replace-nth a-neural-network new-who 5) new-wih 6)))))

           ; matrix -> matrix
           ; Query the neural network
           (query
            (lambda (a-neural-network input-m)
              (matrix-lang (matrix-lang (get-weighted-hidden-output a-neural-network) '*
                                        (matrix-lang (matrix-lang (get-weighted-hidden-input a-neural-network) '*
                                                                  input-m) 'apply activation)) 'apply activation)))

           (is-neural-network-prototype?
            (lambda (x)
              (cond ((and (eq? (length x) 6) (number? (car x)) (number? (car (cdr x))) (number? (car (cddr x))) (number? (car (cdddr x))) (matrix? (car (cdr (cdddr x)))) (matrix? (car (cdr (cdr (cdddr x)))))) #t)
                    (else #f))))

           (try-cast-to-neural-network
            (lambda (x)
              (cond ((is-neural-network-prototype? x) (list ':neural-network x))
                    (else (quote ())))))
           
           )

    ; <pattern> -> <varies>
    ; Grammar for little neural network language
    ; prototype (number number number number matrix matrix))
    ; 'new number number number number    -> neural-network
    ; 'new number number                  -> neural-network
    ; neural-network 'train matrix matrix -> matrix
    ; neural-network 'quert matrix        -> matrix
    (match-lambda*
      ((list 'new (? number? w) (? number? x) (? number? y) (? number? z)) (create-neural-network w x y z))
      ((list 'new (? number? x) (? number? y)) (create-neural-network x x x y))
      ((list (? neural-network? x) 'train (? matrix? y) (? matrix? z)) (train x y z))
      ((list (? neural-network? x) 'query (? matrix? y)) (query x y))
      ((list x 'try-cast-to-neural-network) (try-cast-to-neural-network x))
      (x x)
      (_ #f))))

; Provide access to this module within other files
(provide neural-net-lang)