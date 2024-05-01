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

(((curry2 +) 5) 6)

;;5 curry3---------------------------------------------------------------------------
;; Develop the curry3 function; it takes a function of three arguments, and produces a function
;; that takes one argument and produces a function that takes one argument and produces a function
;; that takes one argument and produces the result of calling the input function on the three
;; given arguments.In other words, it has the type
;; 
;; (All (a b c d) ((a b c -> d) -> (a -> (b -> (c -> d)))))
;; 
;; ... for types a,b,c, and d. You will need lambda for this.

(define (curry3 f)
  (λ (x) (λ (y) (λ (z) (f x y z)))))

((((curry3 +) 5) 10) 15)

;; 6 contains?
;; Develop the contains? function, that consumes a list and a symbol and returns true exactly
;; when the symbol occurs in the list.
(define (contains? l s)
  (cond
    [(empty? l)                 #f]
    [(equal? (first l) s)       #t]
    [else                       (contains? (rest l) s)]))

(contains? (list 'a 'b 'c) 'c)
(contains? (list 'd 'e 'f) 'a)

;; Use curry2 and contains? to develop in-list-many?, that consumes a source list of symbols and
;; a list of query symbols, and returns a list of booleans indicating for the corresponding element
;; of the query list whether it occurs in the source list. Use the built-in function map. This
;; function should be a one-liner.

(define (in-list-many? sl ql)
  (map ((curry2 contains?) sl) ql))

(in-list-many? '(a b c d) '(c e))
(in-list-many? '() '())
(in-list-many? '() '(x))









