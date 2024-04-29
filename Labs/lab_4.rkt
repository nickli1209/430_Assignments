#lang racket
(require rackunit)
;;lab4 in racket

;; 2 applying lambdas---------------------------------------------------------
What should this expression produce?

((lambda (x) (+ x 2)) 3)

Try running it. Did it produce what you expected?

Next: what should this expression produce?

((lambda (f g) (f (g 3)))
 (lambda (x) (+ x 3))
 (lambda (x) (* x 2)))
Try to figure it out first! Then evaluate it.
