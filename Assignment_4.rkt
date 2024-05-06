#lang typed/racket
(require typed/rackunit)
;;ASSIGNMENT 4, NICK LI DREW KIM
;;JUST STARTING... 34 tests failed..

;;TYPES AND STRUCTS---------------------------------------------------------
;;for expressions AST
(define-type ExprC (U numC idC lambC appC strC ifC))
(struct numC ([n : Real])#:transparent)
(struct idC ([name : Symbol])#:transparent)
(struct lambC ([id : (Listof Symbol)] [body : ExprC]) #:transparent)
(struct strC ([str : String])#:transparent)
(struct appC([f : ExprC] [args : (Listof ExprC)])#:transparent)
(struct ifC ([test : ExprC] [then : ExprC] [else : ExprC])#:transparent) 

;;for values
(define-type Value(U numV boolV strV cloV primV))
(struct numV ([n : Real])#:transparent)
(struct boolV ([b : Symbol])#:transparent)
(struct strV ([str : String])#:transparent)
(struct cloV ([params : (Listof Symbol)] [body : ExprC ] [env : Env])#:transparent)
(struct primV ([s : Symbol])#:transparent)

;;for environments
(struct Binding ([name : Symbol] [val : Value])#:transparent)
(define-type Env (Listof Binding))

;;TOP ENVIRONEMNT--------------------------------------------------------------
;;holds all primative type valid in any environment, regardless of locals
;;or lamb params
(define top-env (list (Binding '+ (primV '+))
                      (Binding '- (primV '-))
                      (Binding '* (primV '*))
                      (Binding '/ (primV '/))
                      (Binding '<= (primV '<=))
                      (Binding 'equal? (primV 'equal?))
                      (Binding 'error (primV 'error))
                      (Binding 'true (boolV 'true))
                      (Binding 'false (boolV 'false))))


;;PARSING-------------------------------------------------------------------
;;Main Parse, input sexp, outputs exprC representing AST
(define (parse [exp : Sexp]) : ExprC
  (match exp
    [(? real? n)                  (numC n)]
    [(? symbol? name)             (cond
                                    [(allowed? name)       (idC name)]
                                    [else (error 'parse "ZODE: invalid symbol for an id")])] 
    [(? string? str)              (strC str)]
    [(list 'if ': check ': then ': else)     (ifC (parse check) (parse then) (parse else))]
    [(list 'lamb ': (? symbol? id) ... ': body)      (lambC (cast id (Listof Symbol)) (parse body))]
    [(? list? aps)                    (appC (parse (first aps)) (map parse (rest aps)))]
    [else                         (error 'parse "ZODE: Invalid Zode Syntax")]))


;;CLAUSE PARSING ( helper for parsing part of locals) takes sexp, and env
#;(<CLauses> ::= <id> = <expr>
          || <id> = <expr> : <clauses>)

;;just takes in the clause stuff into a helper called parse-clauses,
;;that disregards the body of a local, and the name of a locasl,
;;so {locals : x =1 ; y=2 : z =3:
;;    {+ {+ x y} z}}
;; so in this case, we will pass x =1 ; y=2 : z =3 to parse-clauses

;(define (parse-clauses ))
 
;;INTERPING----------------------------------------------------------------
;;INTERP EXPRESSIONS (main Interp),input is exp as an ExprC,and env starting with top-env,
;;outputs a value...

(define (interp [exp : ExprC] [env : Env]): Value
  (match exp
    [(numC n)                   (numV n)]
    [(strC str)                 (strV str)]
    [(idC id)                   (lookup id env)]
    [(ifC check then else)      (cond
                                  [(equal? (interp check env) (boolV 'true))   (interp then env)]
                                  [(equal? (interp check env) (boolV 'false))  (interp else env)]
                                  [else (error 'interp"ZODE: if condition not a boolean")])]
    [(lambC params body)          (cloV params body env)] ;;primV are created by bindings in top-env
    [(appC f args)              (define args-int (map (λ (arg) (interp (cast arg ExprC) env)) args))
                                (match (interp f env)
                                  [(cloV params body env)       (if (equal? (length params) (length args-int))
                                                                    (interp body (extend-env params args-int env))
                                                                    (error 'interp "ZODE : Invalid number of arguments in"))]
                                  ;;primV still work in progress, need to finish helpers
                                  [(primV op)                   (cond
                                                                  ;;could do a better check for num args to prims
                                                                  [(> (length args-int) 2) (error 'interp "ZODE : Invalid number of arguments in ~e" op)]
                                                                  [(equal? op 'error)    (prim-error args-int)]
                                                                  ;;need to add case to apply-prims given the op here
                                                                  [else (numV 10)])];; temp to please compiler
                                  [else (error'interp"ZODE: ~e is not a valid application" f)])]))


;;SERIALIZE-----------------------------------------------------------------
;;takes as input a value, return a string representing the value
;;returns either <procedure> or <primativeOp> for primV cloV
(define (serialize [val : Value]) : String
  (match val
    [(numV n)             (~v n)]
    [(strV str)              str]
    [(boolV b)              (cond
                              [(equal? b 'true)  "true"]
                              [(equal? b 'false)  "false"]
                              [else     (error 'serialize "Invalid boolean value ~e" b)])]
    [(cloV params body env)  "#<procedure>"]
    [(primV s)               "#<PrimativeOp>"]))

;;TEST SERIALIZE
(check-equal? (serialize (numV 10)) "10")
(check-equal? (serialize (cloV (list 'x 'y 'z) (appC (idC '+) (list (idC 'x) (numC 10))) '()))"#<procedure>")
(check-equal? (serialize (primV '+)) "#<PrimativeOp>")
(check-equal? (serialize (strV "hello world")) "hello world")
(check-equal? (serialize (boolV 'true)) "true")
(check-equal? (serialize (boolV 'false)) "false")
(check-exn ;;tests not a bolean
 #px"Invalid boolean value 'notabool"
 (λ () (serialize (boolV 'notabool))))



;;TOP-INTERP---------------------------------------------------------------
;;takes as input a sexp, calls parse, then interp, then serialize, returns string
;;representing the result of the code
(define (top-interp [s : Sexp]) : String
  (serialize (interp (parse s) top-env)))


;;ENVIRONMENT STUFF:------------------------------------------------------
;;!!!edit this functionality to handle checl to top-env if not in current env
;;!!!that way wont need a cond everywhere we lookup to check both env's
;;lookup takes as input a symbol representing an idC, and checks the passed
;;environment env for its value, returning a number as of now...
  (define (lookup [for : Symbol] [env : Env]) : Value
    (match env
      ['() (error 'lookup "ZODE: name not found: ~e" for)]
      [(cons (Binding name val) r) (cond
                    [(symbol=? for name) val]
                    [else (lookup for r)])]))


;; GENERAL HELPERS--------------------------------------------------------------

;;ALLOWED?------------------------------------------------
;;allowed? takes as input a sexp and returns ture if the symbol isnt an
;;invalid symbol
(define (allowed? [sym : Sexp]): Boolean
  (not (or 
      (equal? sym 'error)
      (equal? sym 'equal?)
      )))


;;EXTEND_ENV---------------------------------------------
;;extend-env takes as input a list of params, a list of args cooresponding
;;to the params, and a current environment
(define (extend-env [params : (Listof Symbol)] [args : (Listof Value)] [org-env : Env]): Env
  ;;length of args and params already checked equal ininterp
  ;;add check here if needed anywhere other than interp appC
  (define new-env (map Binding params args));;maps each param to each arg in a Binding
  (append new-env org-env))

;;test extend-env



;;PRIMV EVALS---------------------------------------------
;;prim+
(define (prim+ [args : (Listof Value)]): Value
  (if (equal? (length args) 2)
      (let ([a (first args)]
            [b (second args)])
        (if (and (numV? a) (numV? b))
            (numV (+ (numV-n a) (numV-n b)))
            (error 'interp "ZODE: operands must be reals")))
      (error 'interp "ZODE: expects exactly two operands")))


;;prim-
(define (prim- [args : (Listof Value)]): Value
  (numV 0))

;;prim*
(define (prim* [args : (Listof Value)]): Value
  (numV 0))

;;prim/
(define (prim/ [args : (Listof Value)]): Value
  (numV 0))

;;prim<=
(define (prim<= [args : (Listof Value)]): Value
  (numV 0))

;;prim-equal?
(define (prim-equal? [args : (Listof Value)]): Value
  (numV 0))

;;apply-prims, takes as input a Symbol op, and a list of values args
;;applys the appropriate primative operation and returns a Value
(define (apply-prims [op : Symbol] [args : (Listof Value)]): Value
  (match op
    ['+         (prim+ args)]
    ['*         (prim+ args)]
    ['-         (prim+ args)]
    ['/         (prim+ args)]
    ['<=         (prim+ args)]
    ['equal?         (prim+ args)]
    [else         (error 'interp"ZODE: ~e is not a valid operator")])
  )

;test apply-prims

;;prim-error takes as input args, a list of vals, returns an error with the
;;serialized val
;;takes list for simplicity, valid input is only one value to error
(define (prim-error [val : (Listof Value)])
  (error 'user-error"ZODE: user-error ~e" (serialize (first val))))

;;test prim-error



;;TESTCASES---------------------------------------------------------------------
;;------------------------------------------------------------------------------

;;Parse Tests-------------------------------------------------------------------
(check-equal? (parse '{lamb : x y : {+ x 5}}) (lambC (list 'x 'y) (appC (idC '+) (list (idC 'x) (numC 5)))))
(check-equal? (parse '{lamb : x : {+ x {lamb : y : {- y 1}}}})
              (lambC (list 'x) (appC (idC '+) (list (idC 'x) (lambC (list 'y) (appC (idC '-) (list (idC 'y) (numC 1))))))))
(check-equal? (parse '{/ f g} ) (appC (idC '/) (list (idC 'f) (idC 'g))))
(check-equal? (parse "test") (strC "test"))
(check-exn
 #px"ZODE: invalid symbol for an id"
 (λ () (parse 'error)))
(check-equal? (parse '{if : (<= 10 5) : (+ 10 5) : false})
              (ifC (appC (idC '<=) (list (numC 10) (numC 5))) (appC (idC '+) (list (numC 10) (numC 5))) (idC 'false)))
#;(check-exn
 #px"ZODE: Invalid Zode Syntax"
 (λ () (parse (list 3 '& 5))));;with new def of appC, this should get caught as an appC
;;will not error here, will eror in iterp when no value for appC named 3 in env



;;TEST INTERP-------------------------------------------------------------------
(check-equal? (interp (numC 10) '()) (numV 10))
(check-equal? (interp (strC "hello world") '()) (strV "hello world"))
(check-equal? (interp (idC 'x) (list (Binding 'x (numV 10)))) (numV 10))
(check-exn ;;tests lookup on variable not bound in env
 #px"ZODE: name not found"
 (λ () (interp (idC 'y) (list (Binding 'x (numV 10))))))
;;test if
(check-equal? (interp (ifC (idC 'x) (numC 1) (numC 0)) (list (Binding 'x (boolV 'true)))) (numV 1))
(check-equal? (interp (ifC (idC 'x) (numC 1) (numC 0)) (list (Binding 'x (boolV 'false)))) (numV 0))
(check-exn
 #px"ZODE: if condition not a boolean"
 (λ () (interp (ifC (idC 'x) (numC 1) (numC 0)) (list (Binding 'x (numV 10))))))
;;test lamb
(check-equal? (interp (lambC (list 'x 'y 'z) (numC 10)) '()) (cloV (list 'x 'y 'z) (numC 10) '()))


;;TEST TOP-INTERP---------------------------------------------------------------
(check-equal? (top-interp "hello world") "hello world")
(check-equal? (top-interp 10) "10")
;(check-equal? (top-interp 'true) "true")
;(check-equal? (top-interp '{<= 9 10}) "true")



;;HELPER TESTS------------------------------------------------------------------

;;test prim+-----------------------------------------
(check-equal? (prim+ (list (numV 1) (numV 2))) (numV 3))
(check-exn
 #px"ZODE: operands must be reals"
 (λ () (prim+ (list (numV 1) (strV "uhoh")))))
(check-exn
 #px"ZODE: expects exactly two operands"
 (λ () (prim+ (list (numV 1) (numV 2) (numV 3)))))