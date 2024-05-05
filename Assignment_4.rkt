#lang typed/racket
(require typed/rackunit)
;;ASSIGNMENT 4, NICK LI DREW KIM
;;JUST STARTING...
;;TYPES AND STRUCTS---------------------------------------------------------
;;for expressions AST
(define-type ExprC (U numC idC lambC appC strC ifC))
(struct numC ([n : Real])#:transparent)
(struct idC ([name : Symbol])#:transparent)
(struct lambC ([id : (Listof Symbol)] [body : ExprC]) #:transparent)
(struct strC ([str : String])#:transparent)
(struct appC([f : Symbol] [args : (Listof ExprC)])#:transparent)
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
    [(? real? n)               (numC n)]
    [(? symbol? name)        (idC name)] ;;bools should get caught here, then looked up in env in interp
    [(? string? str)         (strC str)]
    [(list 'lamb ': (? symbol? id) ... ': body)      (lambC (cast id (Listof Symbol)) (parse body))]
    [(list (? symbol? f) args ...)       (appC f (map parse args))]))



;;Parse Tests:
(check-equal? (parse '{lamb : x y : {+ x 5}}) (lambC (list 'x 'y) (appC '+ (list (idC 'x) (numC 5)))))
(check-equal? (parse '{lamb : x : {+ x {lamb : y : {- y 1}}}}) (lambC (list 'x) (appC '+ (list (idC 'x) (lambC (list 'y) (appC '- (list (idC 'y) (numC 1))))))))
(check-equal? (parse '{/ f g} ) (appC '/ (list (idC 'f) (idC 'g))))
(check-equal? (parse "test") (strC "test"))
;;(check-equal? (parse 'true) (boolC #t))

;;CLAUSE PARSING ( helper for parsing part of locals) takes sexp, and env
#;(<CLauses> ::= <id> = <expr>
          || <id> = <expr> : <clauses>)

;;just takes in the clause stuff into a helper called parse-clauses,
;;that disregards the body of a local, and the name of a locasl,
;;so {locals : x =1 ; y=2 : z =3:
;;    {+ {+ x y} z}}
;; so in this case, we will pass x =1 ; y=2 : z =3 to parse-clauses


;;INTERPING----------------------------------------------------------------
;;INTERP EXPRESSIONS (main Interp),input is exp as an ExprC,and env starting with top-env,
;;outputs a value...
(define (interp [exp : ExprC] [env : Env]): Value
  (match exp
    [(numC n)          (numV n)]
    [(strC str)        (strV str)]
    [(idC id)          (lookup id env)]));;if an idc is a bool, it will get subbed to true/false here


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
(check-equal? (serialize (cloV (list 'x 'y 'z) (appC '+ (list (idC 'x) (numC 10))) '()))"#<procedure>")
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

;;TEST TOP-INTERP
(check-equal? (top-interp "hello world") "hello world")
(check-equal? (top-interp 10) "10")



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
;;



;;TESTCASES---------------------------------------------------------------------
;;------------------------------------------------------------------------------



;;TEST INTERP-------------------------------------------------------------------
(check-equal? (interp (numC 10) '()) (numV 10))
(check-equal? (interp (strC "hello world") '()) (strV "hello world"))
(check-equal? (interp (idC 'x) (list (Binding 'x (numV 10)))) (numV 10))
(check-exn ;;tests lookup on variable not bound in env
 #px"ZODE: name not found"
 (λ () (interp (idC 'y) (list (Binding 'x (numV 10))))))

