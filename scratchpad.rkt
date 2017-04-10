#lang racket

; I'm just using this file for routine setup stuff while I test

(require "make-neural-network.rkt")
(require "make-matrix.rkt")

(define neural-net (make-neural-network 3 3 3 0.3))

(define matrix ((make-matrix) 3 3))