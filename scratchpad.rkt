#lang racket

; I'm just using this file for routine setup stuff while I test

(require "make-neural-network.rkt")
(require "make-matrix.rkt")

(define neural-net (make-neural-network 3 3 3 0.3))

(define matrix-a (make-matrix 3 3))
(define matrix-b (make-matrix 3 3))

((matrix-a 'set!) '(1 1 1
                    2 2 2
                    3 3 3))

((matrix-b 'set!) '(1 2 3
                    1 2 3
                    1 2 3))

; expect
; 18 18 18
; 18 18 18
; 18 18 18
; ((matrix-a '*) matrix-b)
; ((matrix-a 'get-row) 2)