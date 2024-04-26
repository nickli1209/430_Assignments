#lang typed/racket
(require typed/rackunit)
;;Lab 3

;;parse000-----------------------------------------------------------------------

;;the function parse000 takes as input an s-expression, returns true if
;;the s-expression is a list container a number, the symbol 'chris, and any symbol

(define (parse000 [sex : Sexp]) : Boolean
  (match sex
    [(list (? number?) 'chris (? symbol?)) #t]
    [other                          #f]))

;;tests
(check-equal? (parse000 (list 10 'chris 'pp)) #t)
(check-equal? (parse000 '(10 'chris 10)) #f)
(check-equal? (parse000 (list 10 'chris 10)) #f) ;;ask why this fails...
(check-equal? (parse000 '()) #f)


;;parse001
;;the function parse001 takes as input an s-expression, returns the symbol if
;;the s-expression is a list containing a number, the symbol 'chris, and any symbol

(define (parse001 [sex : Sexp]) : (U Symbol Boolean)
  (match sex
    [(list (? number?) 'chris (? symbol? s)) s]
    [other                          #f]))

;;tests
(check-equal? (parse001 (list 10 'chris 'pp)) 'pp)
(check-equal? (parse001 '(10 'chris 10)) #f)
(check-equal? (parse001 (list 10 'chris 10)) #f) ;;ask why this fails...
(check-equal? (parse001 '()) #f)

;;parse002--------------------------------------------------------------------------
;;the fucntion pasre002 takes an sexp as an input and returns a list l of numbers if the sexp
;;was a list of len 3, where the second item is a list, otherwise false

(define (parse002 [sex : Sexp]) :(U (Listof Real) Boolean)
  (match sex
    ;;list? check if of type list, andmap? on the list l, with procedure r=Real?
    ;;checks if each element of l is a Real
    [(list _ (list (? real? lst) ...) _) (cast lst (Listof Real))] ;;ask how l becomes list of real instead of list of sexp
    [other                          #f]))

;;tests
(check-equal? (parse002 (list 10 (list 1 2 3) 'pp)) (list 1 2 3))
(check-equal? (parse002 (list 10 11 'pp)) #f)
(check-equal? (parse002 "string") #f)


;;ohno (nick)-----------------------------------------------------------------------------------
;;the function ohno accepts a value, and returns the symbol 'okay if the value was a number
;;else an error message is displayed
(define (ohno [val : Any ]) : Symbol
  (if (number? val)
      'okay
      (error 'string-insert "Expected a number, got ~e" val)))
;;tests
(check-equal? (ohno 11) 'okay)
(check-equal? (ohno -4424.24) 'okay)
(check-exn
 #px"Expected a number, got 'hello"
 (λ()(ohno 'hello)))
(check-exn
 #px"Expected a number, got #t"
 (λ()(ohno #t)))

;;Arith Language----------------------------------------------------------------------------
;;changed tstruct to struct not sure what a tstruct is...
(define-type ArithC (U numC plusC multC))
(struct numC ([n : Real])#:transparent)
(struct plusC ([l : ArithC] [r : ArithC])#:transparent)
(struct multC ([l : ArithC] [r : ArithC])#:transparent)

(define (interp [a : ArithC]) : Real
  (match a
    [(numC n) n]
    [(plusC l r) (+ (interp l) (interp r))]
    [(multC  l r) (* (interp l) (interp r))]))


;;Nick Last Night Below----------------------------------------------------------------------
;;evalutation....not sure--------------------------------------------------------------------
(define (evaluation [exp : ArithC]): Real
  (interp exp))

;;tests
(check-= (evaluation (numC 10)) 10 0.01)
(check-= (evaluation (plusC (numC 10) (numC 1))) 11 0.01)
(check-= (evaluation (multC (numC 10) (numC 1))) 10 0.01)
(check-= (evaluation (plusC (numC 1)(plusC (numC 10) (numC 1)))) 12 0.01)

;;num-adds-----------------------------------------------------------------------------------
;;the function num-adds accepts an ArithC, and returns the number of additions it contains
(define (num-adds [exp : ArithC]) : Real
  (printf "Entering num-adds with: ~e\n" exp)
  (match exp
    [(plusC l r)              (+ 1 (num-adds l) (num-adds r))]
    [(multC l r)                (+ (num-adds l) (num-adds r))]
    [else                                                   0]))

;;tests
(check-equal? (num-adds (numC 10)) 0)
(check-equal? (num-adds (plusC (numC 10) (numC 1))) 1)
(check-equal? (num-adds (plusC (numC 1)(plusC (numC 10) (numC 1)))) 2)
(check-equal? (num-adds (multC (numC 1)(plusC (numC 10) (numC 1)))) 1)

;;parser for arith----------------------------------------------------------------------------
;;the function parse-arith takes as input a Sexp, and returns an ArithC, or error if not valid Arith
(define (parse-arith [exp : Sexp]): ArithC
  (match exp
    [(? real? n)        (numC n)]
    [(list '+ l r)     (plusC (parse-arith l) (parse-arith r))]
    [(list '* l r)     (multC (parse-arith l) (parse-arith r))]
    [else     (error 'parse-arith "Invalid Arith Syntax")]))

;;tests
;;why did these tests fail without adding transparent field to the struct defs for pluC etc
;;but none of the test cases above failed...
(check-equal? (parse-arith '(+ 2 3)) (plusC (numC 2) (numC 3)))
(check-equal? (parse-arith '3) (numC 3))
(check-equal? (parse-arith '(+ (* 2 3) 3)) (plusC (multC (numC 2) (numC 3)) (numC 3)))
(check-exn
 #px"Invalid Arith Syntax"
 (λ()(parse-arith '(+ 1))))


;;extended parser for arith (support ^2)-----------------------------------------------------------
;;the function parse-arith takes as input a Sexp, and returns an ArithC, or error if not valid Arith
(define (parse-arith2 [exp : Sexp]): ArithC
  (match exp
    [(? real? n)        (numC n)]
    [(list '+ l r)     (plusC (parse-arith2 l) (parse-arith2 r))]
    [(list '* l r)     (multC (parse-arith2 l) (parse-arith2 r))]
    [(list '^2 n)       (multC (parse-arith2 n) (parse-arith2 n))]
    [else     (error 'parse-arith2 "Invalid Arith Syntax")]))

;;tests
;;why did these tests fail without adding transparent field to the struct defs for pluC etc
;;but not of the test cases above failed...
(check-equal? (parse-arith2 '(+ 2 3)) (plusC (numC 2) (numC 3)))
(check-equal? (parse-arith2 '3) (numC 3))
(check-equal? (parse-arith2 '(+ (* 2 3) 3)) (plusC (multC (numC 2) (numC 3)) (numC 3)))
(check-exn
 #px"Invalid Arith Syntax"
 (λ()(parse-arith2 '(+ 1))))
(check-equal? (parse-arith2 '(^2 3)) (multC (numC 3) (numC 3)))

;;top-interp------------------------------------------------------------------------------------
;;the function top-interp accepts an Sexp in Arith, and returns it evaluated,
;;using parse-arith2 and interp
(define (top-interp [exp : Sexp]): Real
  (interp (parse-arith2 exp)))

;;tests
(check-equal? (top-interp '(+ 2 3)) 5)
(check-equal? (top-interp '(* 2 3)) 6)
(check-equal? (top-interp '(^2 2)) 4)
(check-equal? (top-interp '(+ (* 2 3) (^2 3))) 15)




;;zip function----------------------------------------------------------------------------
;;takes as input two lists of Number of the same length, and returns a new list of lists
;;where each element of the new list, is its self a list/tuple containing the both Numbers from
;;that index in the original lists
(define (pair [n : Number] [m : Number]) : (Listof Number)
  (list n m))

(define (zip [list1 : (Listof Number)] [list2 : (Listof Number)]) : (Listof (Listof Number))
  (if (= (length list1) (length list2))
      (map pair list1 list2)  ; Use 'map' with 'list' to pair elements
      (error "The lists must be of the same length")))  ; Error if lengths differ

;;tests
(check-equal? (zip '(1 3 5 7) '(2 4 6 8)) '((1 2) (3 4) (5 6) (7 8)))
(check-equal? (zip '() '()) '())
(check-exn (regexp (regexp-quote "lists must be of the same length"))
           (lambda () (zip '(1) '(3 4))))


4RUHCT









