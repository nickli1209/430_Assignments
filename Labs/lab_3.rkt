#lang typed/racket
(require typed/rackunit)
;;Lab 3

;;parse000-----------------------------------------------------------------------

;;the function parse000 takes as input an s-expression, returns true if
;;the s-expression is a list container a number, the symbol 'chris, and any symbol

(define (parse000 [sex : Sexp]) : Boolean
  (match sex
    [(list n 'chris s)        (and (number? n) (symbol? s))]
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
    [(list n 'chris s)        (if (and (number? n) (symbol? s)) s #f)]
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
    [(list n l s)        (if (and (list? l) (andmap real? l)) l #f)] ;;ask how l becomes list of real instead of list of sexp
    [other                          #f]))

;;tests
(check-equal? (parse002 (list 10 (list 1 2 3) 'pp)) (list 1 2 3))
(check-equal? (parse002 (list 10 11 'pp)) #f)
(check-equal? (parse002 "string") #f)

;;ohno
;;given a value, returns the symbol 'okay if the value is a number. Otherwise, the function errors with an error message
;;containing the value
(define (ohno value)
  (if(number? value)
     'okay
     (error 'not-a-num "~e is not a number" value)))

;;tests
(check-equal? (ohno 5) 'okay)
(check-exn
 #px"jenny"
 (lambda () (ohno "jenny")))
(check-exn (regexp (regexp-quote "not a num"))
           (lambda () (ohno "not a num")))


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
  (struct numC ([n : Real]))
  (struct plusC ([l : ArithC] [r : ArithC]))
  (struct multC ([l : ArithC] [r : ArithC]))

  #;(define (interp [a : ArithC]) : Real
      (match a
        [(numC n) n]
        [(plusC l r) ...]
        [(multC  l r) ...]))

  ;; in Typed Racket, this produces type errors...:
  #;(define (interp [a : ArithC]) : Real
      (match a
        [(numC n) n]
        [(plusC l r) (+ l r)]
        [(multC  l r) (+ l r)]))

  (define (interp [a : ArithC]) : Real
    (match a
      [(numC n) n]
      [(plusC l r) (+ (interp l) (interp r))]
      [(multC  l r) (* (interp l) (interp r))]))








