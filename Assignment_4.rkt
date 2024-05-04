#lang typed/racket
(require typed/rackunit)
;;ASSIGNMENT 4, NICK LI DREW KIM

;;TYPES AND STRUCTS---------------------------------------------------------
;;for expressions AST
(define-type ExprC (U numC idC lambC boolC appC stringC))
(struct numC([n : Real])#:transparent)
(struct idC ([name : Symbol])#:transparent)


;;for values


;;for environments
;;(struct Binding ([name : symbol] [val: ExprC])#:transparent)
(struct Binding ([name : Symbol] [val : Real]))
(define-type Env (Listof Binding))

;;PARSING-------------------------------------------------------------------
;;Main Parse, input sexp, outputs exprC representing AST


;;CLAUSE PARSING ( helper for parsing part of locals)
#;(<CLauses> ::= <id> = <expr>
          || <id> = <expr> : <clauses>)

;;just takes in the clause stuff into a helper called parse-clauses,
;;that disregards the body of a local, and the name of a locasl,
;;so {locals : x =1 ; y=2 : z =3:
;;    {+ {+ x y} z}}
;; so in this case, we will pass x =1 ; y=2 : z =3 to parse-clauses


;;INTERPING----------------------------------------------------------------
;;INTERP EXPRESSIONS (main Interp),input isexp as an ExprC,and env starting with top-env,
;;outputs a value...







;;ENVIRONMENT STUFF:------------------------------------------------------
;;lookup takes as input a symbol representing an idC, and checks the passed
;;environment env for its value, returning a number as of now...
  (define (lookup [for : symbol] [env : Env]) : number
    (match env
      ['() (error 'lookup "name not found: ~e" for)]
      [(cons (Binding name val) r) (cond
                    [(symbol=? for name) val]
                    [else (lookup for r)])]))


;; GENERAL HELPERS--------------------------------------------------------------