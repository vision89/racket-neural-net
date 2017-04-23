#lang racket
(require math/base)
(require "make-matrix.rkt")

; Author: Dustin Gulley
; Class: CSC 525
; Description: Project 4.  Initialize and return a neural network.
; Due date: April 26 2017r

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

           (activation-prime (lambda (x) (* (/ 1
                                          (+ 1 (expt euler.0 (* x -1))))
                                          (/ (expt (* -1 euler.0) (* -1 x)) (+ 1 (expt euler.0 (* x -1)))))))
                               
           ; matrix matrix -> matrix
           ; Train the neural network
           (train
            (lambda (a-neural-network training-list)
              (let* (
                     ; Forward propogation
                     (input-m (caar training-list))
                     (target-m (car (cdr (car training-list))))
                     (hidden-sum (matrix-lang (get-weighted-hidden-input a-neural-network) '* input-m))
                     (hidden-result (matrix-lang hidden-sum 'apply activation))
                     (output-sum (matrix-lang (get-weighted-hidden-output a-neural-network) '* hidden-result))
                     (output-result (matrix-lang output-sum 'apply activation))

                     ;Back propogation
                     (error-output-layer (matrix-lang (matrix-lang target-m '- output-result)))
                     (delta-output-layer-dot (matrix-lang (matrix-lang output-sum 'apply activation-prime) 'dot error-output-layer))
                     (hidden-output-changes (matrix-lang (matrix-lang (matrix-lang hidden-result 'transpose) 'apply (lambda (x) (* x (get-learning-rate a-neural-network)))) 'apply (lambda (x) (* x delta-output-layer-dot))))
                     (delta-hidden-layer-dot (matrix-lang (matrix-lang (matrix-lang (get-weighted-hidden-output a-neural-network) 'transpose) 'apply (lambda (x) (* x delta-output-layer-dot))) 'dot (matrix-lang hidden-sum 'apply activation-prime)))
                     (input-hidden-changes (matrix-lang (matrix-lang (matrix-lang input-m 'transpose) 'apply (lambda (x) (* x delta-hidden-layer-dot))) 'apply (lambda (x) (* x (get-learning-rate a-neural-network)))))
                     ;who += learning-rate * (output-errors * final-outputs * (1.0 - final-outputs)) * transpose(hidden-outputs)
                     (new-who (matrix-lang (get-weighted-hidden-output a-neural-network) '+ input-hidden-changes))
                     ;whi += learning-rate * (hidden-errors * hidden-outputs * (1.0 - hidden-outputs)) * transpose(input-m)
                     (new-wih (matrix-lang (get-weighted-hidden-input a-neural-network) '+ hidden-output-changes))
                     (new-neural-net (replace-nth (replace-nth a-neural-network new-wih 1) new-who 2)))
                (cond ((null? (cdr training-list)) new-neural-net)
                      (else (train new-neural-net (cdr training-list)))))))

           ; matrix -> matrix
           ; Query the neural network
           (query
            (lambda (a-neural-network input-m)
              (let* (
                     ; Forward propogation
                     (hidden-sum (matrix-lang (get-weighted-hidden-input a-neural-network) '* input-m))
                     (hidden-result (matrix-lang hidden-sum 'apply activation))
                     (output-sum (matrix-lang (get-weighted-hidden-output a-neural-network) '* hidden-result))
                     (output-result (matrix-lang output-sum 'apply activation)))
                output-result)))

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
      ((list (? neural-network? y) 'train z) (train y z))
      ((list (? neural-network? x) 'query (? matrix? y)) (query x y))
      (x (println 'in-identity)x)
      (_ #f))))

; Provide access to this module within other files
(provide neural-net-lang)