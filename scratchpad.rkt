#lang racket

; I'm just using this file for routine setup stuff while I test

(require "make-neural-network.rkt")
(require "make-matrix.rkt")
(require csv-reading)

(define matrix-data-1 (matrix-lang 'new 3 3 '(1 3 1 2 2 3 3 1 2)))
(define matrix-data-2 (matrix-lang 'new 3 2 '(1 2 3 3 2 1 1 1 1)))

;(println 'multiply-matrixes)
;(println (matrix-lang  matrix-data-1 '*  matrix-data-2))

(println 'make-test-neural-net)
(define test-neural-net (neural-net-lang 'new 3 3 3 0.3))
(println 'querying-neural-net) (neural-net-lang test-neural-net 'query matrix-data-1)

(define make-food-csv-reader
  (make-csv-reader-maker
   '((separator-chars            #\,)
     (strip-leading-whitespace?  . #t)
     (strip-trailing-whitespace? . #t))))

(define next-row
  (make-food-csv-reader (open-input-file "mnist_test_10.csv")))

(define list->tuple
 (lambda (x)
   (cond ((null? x) (quote ()))
         (else (cons (string->number (car x)) (list->tuple (cdr x)))))))

(define mnist-tests (quote ()))

(csv-for-each (lambda (x) (set! mnist-tests (cons (matrix-lang 'new 28 28 (list->tuple x)) mnist-tests))) next-row)

(println (matrix-lang (cadr mnist-tests) 'row 5))


