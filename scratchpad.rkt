#lang racket

; I'm just using this file for routine setup stuff while I test

(require "make-neural-network.rkt")
(require "make-matrix.rkt")

(define neural-net (make-neural-network 3 3 3 0.3))

(define wih (make-matrix 3 3)) ; hidden input matrix
(define wio (make-matrix 3 3)) ; hidden output matrix

(wih 'set! (list-of-random-vals 9 100 100 1/2))
(wio 'set! (list-of-random-vals 9 100 100 1/2))

(wih '* wio)

(wio 'whatever)

