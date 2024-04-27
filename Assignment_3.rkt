#lang typed/racket
(require typed/rackunit)
;;assingnment 3
;;not fully implemented

;;STRUCTS AND TYPES
(define-type ExprC (U numC idC binopC FunappC ifleq0?))
(struct binopC ([op : Symbol] [left : ExprC] [right : ExprC])#:transparent)
(struct idC ([name : Symbol])#:transparent)
(struct numC ([n : Real])#:transparent)
(struct FundefC([name : Symbol] [params : (Listof Symbol)] [body : ExprC])#:transparent)
(struct FunappC ([name : Symbol] [args : (Listof ExprC)])#:transparent)
(struct ifleq0? ([check : ExprC] [then : ExprC] [else : ExprC])#:transparent)


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

(define (valid-id? [id : Symbol]) : Boolean
  (if (or (equal? id '+)
          (equal? id '*)
          (equal? id '-)
          (equal? id '/)
          (equal? id 'ifleq0?)
          (equal? id 'def)
          (equal? id ':)) #f #t))

;;parser for expressions from arith, step 1, change to instead parse + and * into
(define (parse [exp : Sexp]): ExprC
  (match exp
    [(? real? n)        (numC n)]
    [(? symbol? id)      (if (valid-id? id) (idC id) (error 'invalid-name "ZODE: ~e symbol not allowed" id))]

    [(list (? symbol? fname) (? real? args) ...)       (cond
                                                         [(valid-id? fname) (FunappC fname (cast args (Listof ExprC)))])]
    
    [(list (? symbol? op) l r)     (if (hash-has-key? op-table op)
                                       (binopC op (parse l) (parse r))
                                       (error 'interp "ZODE: Unknown Operator Error"))] ;;the predicate syntax verifies symbol

    
    [(list 'ifleq0? ': check ': then ': else)     (ifleq0? (parse check) (parse then) (parse else))]
    ;;would have to change to accept non real exprc
    [else     (error 'parse "ZODE: Invalid Syntax")]))

#;(cond
                                     [(hash-has-key? op-table op) (binopC op (parse l) (parse r))])
#;(if (hash-has-key? op-table op)
                                       (binopC op (parse l) (parse r))
                                       (error 'interp "ZODE: Unknown Operator Error"))

;;test parse
(check-equal? (parse '{f 1 2}) (FunappC 'f (list (numC 1) (numC 2))))
(check-equal? (parse 10) (numC 10))
(check-equal? (parse '(+ 10 10)) (binopC '+ (numC 10)(numC 10)))
(check-exn
 #px"ZODE: Unknown Operator Error"
 (λ()(parse '(ld 1 2))))
(check-exn
 #px"ZODE: Invalid Syntax"
 (λ()(parse '(+ 2))))
(check-equal? (parse '{ifleq0? : -5 : 1 : 0}) (ifleq0? (numC -5) (numC 1) (numC 0)))
(check-equal? (parse '{ifleq0? : 5 : (+ 5 -4) : 0}) (ifleq0? (numC 5) (binopC '+ (numC 5) (numC -4)) (numC 0)))
(check-equal? (parse 'a) (idC 'a))
(check-exn
 #px"symbol not allowed"
 (λ () (parse '+)))
(check-exn
 #px"symbol not allowed"
 (λ () (parse '(+ 7 *))))


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
    [(list 'def ': (? symbol? name) ': (? symbol? params) ... ': body)
     (FundefC name (cast params (Listof Symbol)) (parse body))]
    ;;if invalid function def
    [else (error 'parse-fundef "ZODE: Invalid Function Definition")]))

;;previous if case for list of params, save for later
#;(if (andmap symbol? params) (FundefC name params (parse body))
         (error 'parse-fundef "ZODE: Invalid Param"))

;;test parse-fundef...
(check-equal? (parse-fundef '{def : sum : x y : {+ 5 6}}) (FundefC 'sum '(x y) (binopC '+ (numC 5) (numC 6))))
(check-exn
 #px"ZODE: Invalid Function Definition"
 (λ()(parse-fundef '{def : sum  x : {+ x x}})))


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
(define (parse-prog [sexp : Sexp]) : (Listof FundefC)
  (match sexp
    ['()                '()]
    [(cons fst rst)      (cons (parse-fundef fst) (parse-prog rst))]
    [else                (error 'invalid-prog "ZODE: Invalid Program Definition Detected")]))

;;tests
(check-exn
 #px"ZODE: Invalid Function Definition"
 (λ () (parse-prog '{+ 5 4})))
(check-exn
 #px"ZODE: Invalid Program Definition"
 (λ () (parse-prog 'hi)))
(check-equal? (parse-prog '()) '())
(check-equal? (parse-prog '{{def : f : x y : {+ x y}}
                            {def : main : : {- 1 2}}})
              (list (FundefC 'f (list 'x 'y) (binopC '+ (idC 'x) (idC 'y)))
                    (FundefC 'main '() (binopC '- (numC 1) (numC 2)))))


;;top-interp (interp programs call parse prog)
#;(define (top-interp2 [fun-sexps : Sexp]): Real
  (interp-fns (parse-prog fun-sexps)))



;;TEST CASES-------------------------------------------------------------------
;;put all test cases at the botoom after wards, (stylistic choice)
