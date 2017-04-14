#lang racket

; I'm just using this file for routine setup stuff while I test

(require "make-neural-network.rkt")
(require "make-matrix.rkt")

(define neural-net (make-neural-network 3 3 3 0.3))

(define wih (make-matrix 3 3)) ; hidden input matrix
(define wio (make-matrix 3 3)) ; hidden output matrix


(wih '= (list-of-random-vals 9 100 100 1/2))
(wio '= (list-of-random-vals 9 100 100 1/2))

(println 'starting-tests)

; Make test matrixes
; given (1 3 1 2 2 3 3 1 2) * (1 2 3 3 2 1 1 3 2), expect (11 11 8 11 17 14 8 14 14)
(define test-matrix-1 (make-matrix 3 3))
(define test-matrix-2 (make-matrix 3 3))
(define test-matrix-3 (make-matrix 3 3))

(test-matrix-1 '= '(1 3 1 2 2 3 3 1 2))
(test-matrix-2 '= 1 2 3 3 2 1 1 3 2)
(test-matrix-3 '= 1 1 1 1 1 1 1 1 1)
(print 'test-matrix-1:) (println (test-matrix-1))
(print 'test-matrix-2:)(println (test-matrix-2))
(print 'test-matrix-1-data:) (println (test-matrix-1 'as-list))
(print 'test-matrix-2-data:)(println (test-matrix-2 'as-list))
(print 'test-matrix-1*test-matrix-2:)(println ((test-matrix-1 '* test-matrix-2) 'as-list))
(print 'test-matrix-1*test-matrix-2*test-matrix-3:)(println ((test-matrix-1 '* test-matrix-2 '* test-matrix-3) 'as-list))
