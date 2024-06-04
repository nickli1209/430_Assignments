;;LAB 6
#lang typed/racket
(require typed/rackunit)

;;ExprC definition from Assignment 5 here
(define-type ExprC (U numC idC lambC appC strC ifC))
(struct numC ([n : Real])#:transparent)
(struct idC ([name : Symbol])#:transparent)
(struct lambC ([id : (Listof Symbol)] [body : ExprC]) #:transparent)
(struct strC ([str : String])#:transparent)
(struct appC([f : ExprC] [args : (Listof ExprC)])#:transparent)
(struct ifC ([test : ExprC] [then : ExprC] [else : ExprC])#:transparent) 

;;random-symbol function
;;returns one random symbol from a set of 8 selected symbols
(define syms '(a b c d e x y z))

(define (random-symbol)
  (list-ref syms (random 8)))






