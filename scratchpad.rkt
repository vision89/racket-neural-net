#lang racket

; I'm just using this file for routine setup stuff while I test

(require "make-neural-network.rkt")
(require "make-matrix.rkt")

(define matrix-data-1 (matrix-lang 'new 3 3 '(1 3 1 2 2 3 3 1 2)))
(define matrix-data-2 (matrix-lang 'new 3 3 '(1 2 3 3 2 1 1 3 2)))

(println 'make-test-neural-net)
(define test-neural-net (neural-net-lang 'new 3 3 3 0.3))
(println 'querying-neural-net) (neural-net-lang test-neural-net 'query matrix-data-1)

