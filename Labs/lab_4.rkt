#lang racket
(require rackunit)
;;lab4 in racket

;; 2 applying lambdas---------------------------------------------------------
;; What should this expression produce?
;; 
 ((lambda (x) (+ x 2)) 3)
;;should be 5
 
;; 
;; Try running it. Did it produce what you expected?
;; 
;; Next: what should this expression produce?
;; 
  ((lambda (f g) (f (g 3)))
  (lambda (x) (+ x 3))
  (lambda (x) (* x 2)))

;;should eval to 9

;;Try to figure it out first! Then evaluate it.

;;3 curied add-----------------------------------------------------------
;; Develop the curried-add function. It takes a number ’a’ and returns a function
;; that takes a number ’b’ and returns a+b. In other words, it has the type
;; 
;; (number -> (number -> number))
;; 
;; ... where (t1 -> t2) is the type of a function that takes a t1 and produces a t2.

(define (curried-add a)
  (λ (b) (+ a b)))

((curried-add 5)5)

;;4 curry2----------------------------------------------------------------
;; Develop the curry2 function; it takes a function of two arguments f, and produces a
;; function that we’ll call M. The function M takes one argument and produces a function
;; that we’ll call N. N takes one argument and produces the result of calling the input
;; function f on the two given arguments. In other words, it has the type
;; 
;; (All (a b c) ((a b -> c) -> (a -> (b -> c))))
;; 
;; ... for types a,b, and c. You will need lambda for this.

(define (curry2 f)
  (λ (x) (λ (y) (f x y))))

