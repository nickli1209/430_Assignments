#lang typed/racket
(require typed/rackunit)
;;assingnment 3
;;PROGRESS REPORT
;;implemented parse, perse-fundef, and parse-prog
;;original interp from 3.1 is done
;;didnt get to fully implemnting interp, did up to the match for
;;FunappC, wrote the first helper called get-fundefC that returns a
;;FundefC from a function name (symbol) did get to handling subsitution



;;STRUCTS AND TYPES
(define-type ExprC (U numC idC binopC appC ifleq0?))
(struct binopC ([op : Symbol] [left : ExprC] [right : ExprC])#:transparent)
(struct idC ([name : Symbol])#:transparent)
(struct numC ([n : Real])#:transparent)
(struct FundefC([name : Symbol] [params : (Listof Symbol)] [body : ExprC])#:transparent)
(struct appC ([name : Symbol] [args : (Listof ExprC)])#:transparent)
(struct ifleq0? ([check : ExprC] [then : ExprC] [else : ExprC])#:transparent)


;;HELPERS-------------------------------------------------------------------
;;hash map to map the symbol of an operator to the actual operation
 (define op-table(hash
                  '+ +
                  '- -
                  '* *
                  '/ /) )
;; Check if a given symbol is a valid id variable
;;returns true if not one of the symbols below, false if it is
(define (valid-id? [id : Symbol]) : Boolean
  (if (or (equal? id '+)
          (equal? id '*)
          (equal? id '-)
          (equal? id '/)
          (equal? id 'ifleq0?)
          (equal? id 'def)
          (equal? id ':)) #f #t))

;;collection of valif operators for binop
(define valid-ops '(+ - * /))


;; get FundefC takes as input a symbol for a function name and a list of fundefs, and returns,
;;returns a FundefC with the name matching the inputted function name, or error should need this for
;;interp of FunappC...
(define (get-fundefC [name : Symbol] [funs : (Listof FundefC)]) : FundefC
  (cond
  [(empty? funs) (error 'getfundefC"ZODE: Function Name Not Found")]
  [else (cond
     [(equal? name (FundefC-name (first funs))) (first funs)]
     [else (get-fundefC name (rest funs))])]))

;;function to handle substitution


;;TEST get-fundefC
(check-exn
 #px"ZODE: Function Name Not Found"
 (λ () (get-fundefC 'main '())))
(check-equal? (get-fundefC 'main (list (FundefC 'main '() (binopC '+ (numC 5) (numC 5)))))
              (FundefC 'main '() (binopC '+ (numC 5) (numC 5))) )
(check-equal? (get-fundefC 'main (list (FundefC 'f '(x y) (binopC '+ (idC 'x) (idC 'y)))
                                       (FundefC 'main '() (binopC '+ (numC 5) (numC 5)))))
              (FundefC 'main '() (binopC '+ (numC 5) (numC 5))) )



;;3.1------------------------------------------------------------------------

;;PARSE
;;parser for expressions from arith, step 1, change to instead parse + and * into
(define (parse [exp : Sexp]): ExprC
  (match exp
    [(? real? n)        (numC n)]
    [(? symbol? id)      (if (valid-id? id) (idC id) (error 'invalid-name "ZODE: ~e symbol not allowed" id))]
    ;;only can hit binopC if op is valid
    [(list (?  (λ (op) (member op valid-ops)) op) l r) (binopC op (parse l) (parse r))]
    [(list 'ifleq0? ': check ': then ': else)     (ifleq0? (parse check) (parse then) (parse else))]
    ;;would have to change to accept non real exprC as arg
    [(list (? symbol? fname) args ...) (cond
                                         [(member fname valid-ops) (error 'parse "ZODE: Invalid Syntax")]
                                         [(valid-id? fname) (appC fname (map parse args))]
                                         [else (error 'parse "ZODE: Invalid Function Name")])]
    [else     (error 'parse "ZODE: Invalid Syntax")]))




;;PARSE TESTS
(check-equal? (parse '{f 1 2}) (appC 'f (list (numC 1) (numC 2))))
(check-equal? (parse 10) (numC 10))
(check-equal? (parse '(+ 10 10)) (binopC '+ (numC 10)(numC 10)))
;;shouod be numC 1 2 but was a real when list constructed
(check-equal? (parse '(sum 1 2)) (appC 'sum (list (numC 1) (numC 2))))
(check-exn
 #px"ZODE: Invalid Syntax"
 (λ()(parse '(+ 2))))
(check-exn
 #px"ZODE: Invalid Function Name"
 (λ()(parse '(: 2))))
(check-exn
 #px"ZODE: Invalid Syntax"
 (λ()(parse '(2 + 6))))
(check-equal? (parse '{ifleq0? : -5 : 1 : 0}) (ifleq0? (numC -5) (numC 1) (numC 0)))
(check-equal? (parse '{ifleq0? : 5 : (+ 5 -4) : 0}) (ifleq0? (numC 5) (binopC '+ (numC 5) (numC -4)) (numC 0)))
(check-equal? (parse 'a) (idC 'a))
(check-exn
  #px"symbol not allowed"
  (λ () (parse '+)))
 (check-exn
  #px"symbol not allowed"
  (λ () (parse '(+ 7 *))))



;;ORIGINAL INTERP
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



;;Original Top Interp
;;Top-Interp combines parse and interp
(define (top-interpOld [exp : Sexp]): Real
  (interpOld (parse exp)))


;;test top interpOld
(check-equal? (top-interpOld (+ 5 6)) 11)
(check-equal? (top-interpOld (/ 5 5)) 1)



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


;;test parse-fundef...
(check-equal? (parse-fundef '{def : sum : x y : {+ 5 6}}) (FundefC 'sum '(x y) (binopC '+ (numC 5) (numC 6))))
(check-exn
 #px"ZODE: Invalid Function Definition"
 (λ()(parse-fundef '{def : sum  x : {+ x x}})))




;;interp, havent handled FunappC's yet, confused and out of time
(define (interp [exp : ExprC] [funs : (Listof FundefC )]) : Real
    (match exp
      [(numC n) n]
      ;;change to cond to support diide by zero error...
      [(binopC op l r) (if (hash-has-key? op-table op);;check if in op is valid
                           ((hash-ref op-table op) (interp l funs) (interp r funs))
                           (error 'interp "ZODE: Unknown Operator Error"))]
      [(ifleq0? ch t e) (if (<= (interp ch funs) 0) (interp t funs) (interp e funs))]
      [(idC sym)            (error 'interp "ZODE: Interp Tried to Evaluate A Variable")]
      ;;[(FunappC )... todo]
      ))


;;Interp Tests
(check-equal? (interp (ifleq0? (numC -5) (numC 1) (numC 0)) '()) 1)
(check-equal? (interp (ifleq0? (numC 5) (numC 1) (numC 0)) '()) 0)

(check-equal? (interp (binopC '+ (numC 2) (numC 3)) '()) 5)
(check-exn
 #px"ZODE: Unknown Operator Error"
 (λ()(interp (binopC 'y (numC 2) (numC 3)) '())))
(check-exn
 #px"ZODE: Interp Tried to Evaluate A Variable"
 (λ()(interp (idC 'pp) '())))


;;parser for programs--------------------------------------------------
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


;;top-interp (interp programs call parse prog)----------------------------------
;;cant test yet
(define (top-interp [fun-sexps : Sexp]): Real
  ;;(interp-fns (parse-prog fun-sexps))
  1)


;;tests
(check-equal?(top-interp '(test)) 1)


;;TEST CASES-------------------------------------------------------------------
;;put all test cases at the botoom after wards, (stylistic choice)
