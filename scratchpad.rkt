#lang racket

; I'm just using this file for routine setup stuff while I test

(require "make-neural-network.rkt")
(require "make-matrix.rkt")

(define neural-net (make-neural-network 3 3 3 0.3))

(define matrix-a (make-matrix 3 3))
(define matrix-b (make-matrix 3 3))
(define matrix-c (make-matrix 2 3))

((matrix-a 'set!) '(1 3 1
                    2 2 3
                    3 1 2))

((matrix-b 'set!) '(1 2 3
                    3 2 1
                    1 3 2))

((matrix-c 'fill-with-random-vals) 0 4)

; ((matrix-a '*) matrix-b) -> (11 11 8
;                              11 17 14
;                              8  14 14)

;
;
; ((matrix-a 'set-row-col!) 2 2 3) -> (1 3 1 2 3 3 3 1 2)

; expect
;
; ((matrix-a '*) matrix-b)
; ((matrix-a 'get-row) 2)
; ((matrix-a 'get-col) 1)