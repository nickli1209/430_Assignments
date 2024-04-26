#lang typed/racket
(require typed/rackunit)
;;assingnment 3
;;not fully implemented

;;STRUCTS AND TYPES
(define-type ExprC (U numC var binopC FunappC))
(struct binopC ([op : Symbol] [left : ExprC] [right : ExprC])#:transparent)
(struct var ([name : Symbol])#:transparent)
(define-type ArithC (U numC plusC multC))
(struct numC ([n : Real])#:transparent)
(struct plusC ([l : ArithC] [r : ArithC])#:transparent)
(struct multC ([l : ArithC] [r : ArithC])#:transparent)
(struct FundefC([name : Symbol] [params : (Listof Symbol)] [body : ExprC]))
(struct FunappC ([name : Symbol] [args : (Listof ExprC)]))


;;ExprC Concrete Syntax
;;<expr> ::=  <numC> 
;;            {<binopC> <expr> <expr>
;;            {ifleq0? : <expr> : <expr> : <expr>} if less or equal to 0,
;;            <id>

;;3.1------------------------------------------------------------------------
;;worry about later at intepr time, but use map or function to grab proper operation from
;;symbol in Binop....
 (define op-table(hash
                  '+ +
                  '- -
                  '* *
                  '/ /) )

;;parser for expressions from arith, step 1, change to instead parse + and * into
(define (parse [exp : Sexp]): ExprC
  (match exp
    [(? real? n)        (numC n)]
    ;;maybe take out how to handle vars?
    [(? symbol? s)              (var s)]
    [(list (? symbol? op) l r)     (if (hash-has-key? op-table op)
                                       (binopC op (parse l) (parse r))
                                       (error 'interp "ZODE: Unknown Operator Error"))] ;;the predicate syntax verifies symbol
    [else     (error 'parse "ZODE: Invalid Syntax")]))

;;test parse
(check-equal? (parse 10) (numC 10))
(check-equal? (parse '(+ 10 10)) (binopC '+ (numC 10)(numC 10)))
(check-exn
 #px"ZODE: Unknown Operator Error"
 (λ()(parse '(ld 1 2))))
(check-exn
 #px"ZODE: Invalid Syntax"
 (λ()(parse '(+ 2))))


;;interpreter from arith, adjusted to interp binop
;;interp handling error for symbols, shoudl parse instead?
(define (interpOld [ast : ExprC]) : Real
    (match ast
      [(numC n) n]
      [(binopC op l r) (if (hash-has-key? op-table op);;check if in op is valid
                           ((hash-ref op-table op) (interpOld l) (interpOld r))
                           (error 'interpOld "ZODE: Unknown Operator Error"))]));;single binop interp

;;test interp
(check-equal? (interpOld (numC 10)) 10)
(check-equal? (interpOld (binopC '+ (numC 10)(numC 10))) 20)
(check-exn
 #px"ZODE: Unknown Operator Error"
 (λ()(interpOld (binopC 'hd (numC 2)(numC 3)))))


;;Top-Interp combines parse and interp
(define (top-interp [exp : Sexp]): Real
  (interpOld (parse exp)))


;;test top interp
(check-equal? (top-interp (+ 5 6)) 11)
(check-equal? (top-interp (/ 5 5)) 1)



;;3.2 functions with 0 or more args-----------------------------------------------
;;types and definitions changed above in TYPES section

;;parser for functions, function definitions are in the form
;;{def : <name> : <params> : <body>}
(define (parse-fundef [s : Sexp]) : FundefC
  (match s
    ;;currently only handling one param, types werent working with multiple
    [(list 'def ': (? symbol? name) ': (? symbol? params) ': (? list? body))
     (FundefC name (list params) (parse body))]
    ;;if invalid function def
    [else (error 'parse-fundef "ZODE: Invalid Function Definition")]))

;;previous if case for list of params, save for later
#;(if (andmap symbol? params) (FundefC name params (parse body))
         (error 'parse-fundef "ZODE: Invalid Param"))

;;test parse-fundef...
(check-equal? (parse-fundef '{def : sum : x : {+ x x}}) (FundefC 'sum (list 'x) (binopC '+ (var 'x) (var 'x))))
(check-exn
 #px"ZODE: Invalid Function Definition"
 (λ()(parse-fundef '{def : sum : x : {+ x x}})))


;;interp-fns to do interp function defs


;;new interp (expressions.. workmin progress, need to support function apps
#;(define (interp [ast : ExprC] [funs : (Listof FundefC )]) : Real
    (match ast
      [(numC n) n]
      [(binopC op l r) (if (hash-has-key? op-table op);;check if in op is valid
                           ((hash-ref op-table op) (interp l) (interp r))
                           (error 'interp "ZODE: Unknown Operator Error"))]
      [function match   ...     ...]))









;; work with later------------------------------------------------------------

;;parser for programs
#;(define (parse-prog [sexp : Sexp]) : (Listof FundefC))


;;top-interp (interp programs call parse prog)
#;(define (top-interp2 [fun-sexps : Sexp]): Real
  (interp-fns (parse-prog fun-sexps)))



;;TEST CASES-------------------------------------------------------------------
;;put all test cases at the botoom after wards, (stylistic choice)
