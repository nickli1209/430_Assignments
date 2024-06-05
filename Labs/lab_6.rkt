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

(define (random-symbol) : Symbol
  (list-ref syms (random (length syms))))

;;random-base-term function
;;generates a random expression
;;chooses randomly from the set of non-referential expressions
;;uses random-symbol to create the identifiers


(define (random-base-term) : ExprC
  (define idx (random 3))
  (cond
    [(= 0 idx)        (numC (random 100))]
    [(= 1 idx)        (idC (random-symbol))]
    [else             (strC (format "str~a" (random 100)))]))


;;random-term function
;;accepts a maximum depth, returns an expression tree not exceeding
;;max depth

(define (random-term [max-depth : Natural]) : ExprC
  (if (= max-depth 0)
      (random-base-term)
      (let ([exp-idx (random 3)])
         (cond
           [(= exp-idx 0)     (ifC (random-term (- max-depth 1))
                                   (random-term (- max-depth 1))
                                   (random-term (- max-depth 1)))]
           [(= exp-idx 1)     (let ([num-syms (random 4)])
                                (lambC (make-syms num-syms) (random-term (- max-depth 1))))]
           [else              (let ([num-args (random 4)])
                                (appC (random-term (- max-depth 1)) (make-args num-args (- max-depth 1))))]))))

;;random-term helper function
;;generate args for lambs and function apps
(define (make-syms [n : Natural]) : (Listof Symbol)
  (cond
    [(= n 0)      '()]
    [else         (cons (random-symbol) (make-syms (- n 1)))]))

(define (make-args [n : Natural] [depth : Natural]) : (Listof ExprC)
  (cond
    [(= n 0)      '()]
    [else         (cons (random-term depth) (make-args (- n 1) depth))]))

(random-term 3)

;;unparse function here...

(define (unparse [exp : ExprC]): Sexp
  (match exp
    [(numC n)            n]
    [(strC s)            s]
    [(idC x)             x]
    [(lambC params body)      (append (list 'lamb ':) params (list ': (unparse body)))]
    [(appC f args)            (append (list (unparse f)) (map unparse args))]
    [(ifC test then else)     (list 'if ': (unparse test) ': (unparse then) ': (unparse else))]))


;;test unparse
(check-equal? (unparse (numC 10)) 10)
(check-equal? (unparse (idC 'a)) 'a)
(check-equal? (unparse (lambC (list 'a 'b 'c) (strC "nick"))) '{lamb : a b c : "nick"})
(check-equal? (unparse (appC (idC '+) (list (numC 10) (numC 5)))) '{+ 10 5})
(check-equal? (unparse (ifC (idC 'x) (numC 10) (numC 0))) '{if : x : 10 : 0})




