#lang typed/racket
(require typed/rackunit)
;;ZODE 7, typechecking
;;just starting, nothing new implimented...
;;TYPES AND STRUCTS---------------------------------------------------------
;;for expressions AST
(define-type ExprC (U numC idC lambC appC strC ifC))
(struct numC ([n : Real])#:transparent)
(struct idC ([name : Symbol])#:transparent)
(struct lambC ([params : (Listof Symbol)] [paramTs : (Listof Ty)] [retT : (Option Ty)] [body : ExprC])#:transparent)
;;(struct lambC ([params : (Listof Symbol)] [body : ExprC]) #:transparent)
(struct strC ([str : String])#:transparent)
(struct appC([f : ExprC] [args : (Listof ExprC)])#:transparent)
(struct ifC ([test : ExprC] [then : ExprC] [else : ExprC])#:transparent)
(struct local-recC ([id : Symbol] [lamb-def : ExprC] [body : ExprC]) #:transparent)

;;for values
(define-type Value(U numV boolV strV cloV primV))
(struct numV ([n : Real])#:transparent)
(struct boolV ([b : Symbol])#:transparent)
(struct strV ([str : String])#:transparent)
(struct cloV ([params : (Listof Symbol)] [body : ExprC ] [env : Env])#:transparent)
(struct primV ([s : Symbol])#:transparent)

;;for environments
(struct Binding ([name : Symbol] [val : Value])#:transparent)
(define-type Env (Listof Binding)) ;;this may need to include the tenv as well

;;for types
(define-type Ty (U numT boolT strT funT))
(struct numT ()#:transparent)
(struct boolT ()#:transparent)
(struct strT ()#:transparent)
(struct funT ([params : (Listof Ty)] [return : Ty])#:transparent)

(define-type TEnv (Listof TBinding))
(struct TBinding ([name : Symbol] [type : Ty])#:transparent)

;**need to impliment num-eq? , str-eq? , substring
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
                      (Binding 'false (boolV 'false))
                      (Binding 'num-eq? (primV 'num-eq?))
                      (Binding 'str-eq? (primV 'str-eq?))
                      (Binding 'substring (primV 'substring))))

;;base type environment, holds the type requirements for the primV's
(define base-tenv (list (TBinding '+ (funT (list (numT) (numT)) (numT)))
                        (TBinding '- (funT (list (numT) (numT)) (numT)))
                        (TBinding '* (funT (list (numT) (numT)) (numT)))
                        (TBinding '/ (funT (list (numT) (numT)) (numT)))
                        (TBinding '<= (funT (list (numT) (numT)) (boolT)))
                        (TBinding 'num-eq? (funT (list (numT) (numT)) (boolT)))
                        (TBinding 'str-eq? (funT (list (strT) (strT)) (boolT)))
                        (TBinding 'substring (funT (list (strT) (numT) (numT)) (strT)))
                        (TBinding 'true (boolT))
                        (TBinding 'false (boolT))))

;;todo parse:
;**will need to add parse clause for local-rec
;**adjust lamb parse to match tpye checker
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
    [(list 'lamb ': type-param-pair ... '-> rtype ': body)  (parse-lamb (cast type-param-pair (Listof Sexp)) rtype body)]
    ;;**add local-rec parse here
    [(list 'locals ': clause ... ': ex)        (if (has-dups? (parse-clause-ids (cast clause (Listof Sexp))))
                                                  (error 'parse"ZODE: Locals can't have duplicate clauses")
                                                  (appC
                                                   (lambC
                                                    (parse-clause-ids (cast clause (Listof Sexp)))
                                                    (parse-clause-types (cast clause (Listof Sexp)))
                                                    #f
                                                    (parse ex))
                                                   (parse-clause-vals (cast clause (Listof Sexp)))))]
    ;; probably not the best solution for invalid lamb and local but def easiest
    [(list 'locals ': clause ... ':)           (error 'parse"ZODE: Locals must have a body expression")]
    [(list 'lamb ': _ ...)                            (error 'parse"ZODE: Invalid syntax for lamb")]
    ;;maybe need some error checking here for appC parse, 
    [(? list? aps)                    (appC (parse (first aps)) (map parse (rest aps)))]
    [else                         (error 'parse "ZODE: Invalid Zode Syntax")]))


;;parse-lamb
(define (parse-lamb [tpair : (Listof Sexp)] [rtype : Sexp] [body : Sexp]): ExprC
  (define params (map (λ (pair)
                       (match pair
                         [(list type sym)
                          (if (symbol? sym)
                              sym
                              (error 'parse-lamb "ZODE: Invalid parameter to lamb"))]))
                      tpair))
  (define ptypes (map (λ (pair)
                       (match pair
                         [(list type sym)
                          (parse-type (cast type Sexp))]))
                     tpair))
  (if (and (not (has-dups? params))(equal? (length params) (length ptypes)))
      (lambC params ptypes (parse-type rtype) (parse body))
      (error 'parse-lamb"ZODE: Invalid Syntax or Params to lamb")))

;;PARSE-TYPE---------------------------------------
;;**ask about the cast for funT parse
(define (parse-type [exp : Sexp]): Ty
  (match exp
    ['num (numT)]
    ['str (strT)]
    ['bool (boolT)]
    [(list ptypes ... '-> rtype)   (funT (map parse-type (cast ptypes (Listof Sexp))) (parse-type rtype))]
    [else (error 'parse-types"ZODE: Invalid Type Syntax")]))


;;PARSE LOCALS HELPERS---------------------------------------------------
;;helper parse function, parses a clause and returns the list of ids
(define (parse-clause-ids [clauses : Sexp]) : (Listof Symbol)
  (match clauses
    ;;single clause case
    [(list _ (? symbol? id) '= _)        (if
                                         (allowed? id)
                                         (cons id '())
                                         (error 'invalid-id "ZODE: Invalid Identifier Name"))]
    ;;multiple clauses
    [(list _ (? symbol? id) '= _ ': more-clauses ...)       (if (allowed? id)
                                                            (cons id (parse-clause-ids more-clauses))
                                                            (error 'invalid-id "ZODE: Invalid Identifier Name"))]
    [else (error 'clause-syn "ZODE: Invalid clause syntax")]))

;;helper parse function, parses a clause and returns the list of vals
(define (parse-clause-vals [clauses : Sexp]) : (Listof ExprC)
  (match clauses
    [(list _ _ '= val)                            (cons (parse val) '())]
    [(list _ _ '= val ': more-clauses ...)            (cons (parse val) (parse-clause-vals more-clauses))]))

;;helper parse-clause-types
(define (parse-clause-types [clauses : Sexp]): (Listof Ty)
  (match clauses
    [(list type _ '= _)       (cons (parse-type type)'())]
    [(list type _ '= _ ': more-clauses ...)  (cons (parse-type type) (parse-clause-types more-clauses))]
    [else                     (error 'parse-locals"ZODE: Invalid syntax in locals clause")]))


;;TYPE_CHECKING------------------------------------------------------------
;;"type checking should happen after parsing and before interpretation"
;;for lambC, need a new helper which extends the type environment with the argument type
;;then type check on body and compare to return type, then return a funT
(define (type-check [exp : ExprC] [tenv : TEnv]) : Ty
  (match exp
    [(numC n)                    (numT)]
    [(idC id)                    (ty-lookup id tenv)]
    [(strC s)                    (strT)]
    [(ifC cond then else)        (if (equal? (type-check cond tenv) (boolT))
                                     (if (equal? (type-check then tenv) (type-check else tenv))
                                         (type-check then tenv)
                                         (error 'type-check
                                                "ZODE: then and else statements do not match"))
                                     (error 'type-check "ZODE: if condition not a boolean"))]
    [(lambC args argTs retT body)        (if (equal? (type-check body (extend-tenv args argTs tenv)) retT)
                                              (funT argTs retT)
                                              (error 'type-check "ZODE: lamb type mismatch"))]
    ;;maybe doe this for lambC:
                                          #;(if(equal? retT #f)
                                             (type-check body (extend-tenv args argTs tenv))
                                             (if (equal? (type-check body (extend-tenv args argTs tenv)) retT)
                                              (funT argTs retT)
                                              (error 'type-check "ZODE: lamb type mismatch")))
    ;;example: lamb : [{num -> str}]
    [(appC f args)               (let ([funt (type-check f tenv)]
                                       [argts (map (λ (arg) (type-check (cast arg ExprC) tenv)) args)])
                                   (cond
                                     [(not (funT? funt))
                                      (error 'type-check "ZODE: non function type applied to arguments")]
                                     [(not (equal? (funT-params funt) argts))
                                      (error 'type-check "ZODE: arguments not of compatible type to function")]
                                     [else (funT-return funt)]))]))

;;EXTEND TYPE ENVIRONMENT
(define (extend-tenv [params : (Listof Symbol)] [tys : (Listof Ty)] [tenv : TEnv]) : TEnv
  (define new-tenv (map TBinding params tys))
  (append new-tenv tenv))

;;TYPE ENVIRONMENT LOOKUP-------------------------------------------------------
;;same as lookup but for types
;;returns a Ty instead of a Value
(define (ty-lookup [for : Symbol] [tenv : TEnv]) : Ty
  (match tenv
    ['()   (error 'ty-lookup "ZODE: name not found in type environment: ~e" for)]
    [(cons (TBinding name ty) r)    (cond
                                      [(symbol=? for name) ty]
                                      [else (ty-lookup for r)])]))
 
;;INTERPING----------------------------------------------------------------
;;INTERP EXPRESSIONS (main Interp),input is exp as an ExprC,and env starting with top-env,
;;outputs a value...

(define (interp [exp : ExprC] [env : Env]): Value
  (match exp
    [(numC n)                   (numV n)]
    [(strC str)                 (strV str)]
    [(idC id)                   (lookup id env)]
    [(ifC check then else) (cond
                             [(equal? (interp check env) (boolV 'true))   (interp then env)]
                             [(equal? (interp check env) (boolV 'false))  (interp else env)]
                             [else (error 'interp"ZODE: if condition not a boolean")])]
    [(lambC params types rtype body)   (cloV params body env)] ;;primV are created by bindings in top-env
    [(appC f args)         (define args-int (map (λ (arg) (interp (cast arg ExprC) env)) args))
                           (match (interp f env)
                             [(cloV params body env)
                              (if (equal? (length params) (length args-int))
                                  (interp body (extend-env params args-int env))
                                  (error 'interp "ZODE : Invalid number of arguments in function call"))]
                             ;;primV still work in progress, need to finish helpers
                             [(primV op)       (cond
                                                 [(equal? op 'error)    (prim-error args-int)]
                                                 ;;[(equal? op 'println)   (prim-println args-int)]
                                                 ;;need to add case to apply-prims given the op here
                                                 [else (apply-prims op args-int)])]
                             [else (error'interp (format "ZODE: ~a is not a valid application"
                                                         (serialize (interp f env))))])]))


;;SERIALIZE-----------------------------------------------------------------
;;takes as input a value, return a string representing the value
;;returns either <procedure> or <primativeOp> for primV cloV
(define (serialize [val : Value]) : String
  (match val
    [(numV n)             (~v n)]
    [(strV str)           (~v str)]
    [(boolV b)              (cond
                              [(equal? b 'true)  "true"]
                              [(equal? b 'false)  "false"]
                              [else     (error 'serialize "Invalid boolean value ~e" b)])]
    [(cloV params body env)  "#<procedure>"]
    [(primV s)               "#<primop>"]))



;;TOP-INTERP---------------------------------------------------------------
;;takes as input a sexp, calls parse, then interp, then serialize, returns string
;;representing the result of the code
(define (top-interp [s : Sexp]) : String
  (let ([parsed (parse s)])
    (type-check parsed base-tenv)
    (serialize (interp (parse s) top-env))))

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

;;has-dups? takes a list of symbols, returns true if there are duplicates in the list
;;false otherwise
(define (has-dups? [lst : (Listof Symbol)]): Boolean
  (match lst
    ['()   #f]
    [(cons f r)     (if (member f r)
                        #t
                        (has-dups? r))]))

;;ALLOWED?------------------------------------------------
;;allowed? takes as input a sexp and returns ture if the symbol isnt an
;;invalid symbol
(define (allowed? [sym : Sexp]): Boolean
  (not (or 
      (equal? sym 'if)
      (equal? sym 'lamb)
      (equal? sym 'locals)
      (equal? sym ':)
      (equal? sym '=)
      (equal? sym 'local-rec)
      (equal? sym '->)
      )))

;;EXTEND_ENV---------------------------------------------
;;extend-env takes as input a list of params, a list of args cooresponding
;;to the params, and a current environment
(define (extend-env [params : (Listof Symbol)] [args : (Listof Value)] [org-env : Env]): Env
  ;;length of args and params already checked equal ininterp
  ;;add check here if needed anywhere other than interp appC
  (define new-env (map Binding params args));;maps each param to each arg in a Binding
  ;;flipped order of appends, to allow for prim ops as variables, that way looks up local before top-env
  (append new-env org-env))

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
  (if (equal? (length args) 2)
      (let ([a (first args)]
            [b (second args)])
        (if (and (numV? a) (numV? b))
            (numV (- (numV-n a) (numV-n b)))
            (error 'interp "ZODE: operands must be reals")))
      (error 'interp "ZODE: expects exactly two operands")))

;;prim*
(define (prim* [args : (Listof Value)]): Value
  (if (equal? (length args) 2)
      (let ([a (first args)]
            [b (second args)])
        (if (and (numV? a) (numV? b))
            (numV (* (numV-n a) (numV-n b)))
            (error 'interp "ZODE: operands must be reals")))
      (error 'interp "ZODE: expects exactly two operands")))

;;prim/
(define (prim/ [args : (Listof Value)]): Value
  (if (equal? (length args) 2)
      (let ([a (first args)]
            [b (second args)])
        (if (and (numV? a) (numV? b))
            (if (equal? (numV-n b) 0)
                (error 'interp"ZODE:Divide by zero undefined")
                (numV (/ (numV-n a) (numV-n b))))
            (error 'interp "ZODE: operands must be reals")))
      (error 'interp "ZODE: expects exactly two operands")))

;;prim<=
(define (prim<= [args : (Listof Value)]): Value
  (if (equal? (length args) 2)
      (let ([a (first args)]
            [b (second args)])
        (if (and (numV? a) (numV? b))
            (if (<= (numV-n a) (numV-n b))
                (boolV 'true)
                (boolV 'false))
            (error 'interp "ZODE: operands must be reals")))
      (error 'interp "ZODE: expects exactly two operands")))

;;prim-num-eq?
(define (prim-num-eq? [args : (Listof Value)]): Value
  (if (equal? (length args) 2)
      (let ([a (first args)]
            [b (second args)])
        (if (equal?  a  b)
            (boolV 'true)
            (boolV 'false))) 
      (error 'interp "ZODE: num-eq? expects exactly two operands")))

;;prim-str-eq?
(define (prim-str-eq? [args : (Listof Value)]): Value
  (if (equal? (length args) 2)
      (let ([a (first args)]
            [b (second args)])
        (if (equal?  a  b)
            (boolV 'true)
            (boolV 'false))) 
      (error 'interp "ZODE: str-eq? expects exactly two operands")))

;;prim-substring
;;substring takes a string and a start and endinf index and returns the
;;cooresponding substring
(define (prim-substring [args : (Listof Value)]): strV
  (match args
    [(list (strV str) (numV (? exact-integer? s)) (numV (? exact-integer? e))) (strV (substring str s e))]
    [else (error 'interp"ZODE: substring must take three arguement, string, int, int")]))


;;apply-prims, takes as input a Symbol op, and a list of values args
;;applys the appropriate primative operation and returns a Value
(define (apply-prims [op : Symbol] [args : (Listof Value)]): Value
  (match op
    ['+         (prim+ args)]
    ['*         (prim* args)]
    ['-         (prim- args)]
    ['/         (prim/ args)]
    ['<=         (prim<= args)]
    ['num-eq?    (prim-num-eq? args)]
    ['str-eq?    (prim-str-eq? args)]
    ['substring    (prim-substring args)]
    [else         (error 'interp"ZODE: ~e is not a valid operator" op)])
  )


;;prim-error takes as input args, a list of vals, returns an error with the
;;serialized val
;;takes list for simplicity, valid input is only one value to error
(define (prim-error [val : (Listof Value)])
  (if (equal? (length val) 1)
      (error 'user-error(format "ZODE: user-error ~a" (serialize (first val))))
      (error 'interp"ZODE: error function takes only 1 input")))


;;serialize 2, no /'s
(define (serialize2 [val : Value]) : String
  (match val
    [(numV n)             (~v n)]
    [(strV str)           str]
    [(boolV b)              (cond
                              [(equal? b 'true)  "true"]
                              [(equal? b 'false)  "false"]
                              [else     (error 'serialize2 "Invalid boolean value ~e" b)])]
    [(cloV params body env)  "#<procedure>"]
    [(primV s)               "#<primop>"]))


;;TESTCASES---------------------------------------------------------------------
;;------------------------------------------------------------------------------


;;PARSE_TESTS-------------------------------------------------------------------
#;(check-equal? (parse '{lamb : [num x] [num y] -> num : {+ x 5}})
              (lambC (list 'x 'y) (list (numT) (numT)) (numT) (appC (idC '+) (list (idC 'x) (numC 5)))))
(check-equal? (parse '{lamb : [num x] [num y] -> num : {+ x 5}})
              (lambC (list 'x 'y) (list (numT) (numT)) (numT)(appC (idC '+) (list (idC 'x) (numC 5)))))

(check-equal? (parse '{lamb : [num x] -> {num -> num} : {+ x {lamb : [num y] -> num : {- y 1}}}})
              (lambC (list 'x) (list (numT)) (funT (list (numT)) (numT)) (appC (idC '+) (list (idC 'x)
                                                    (lambC (list 'y) (list (numT)) (numT)(appC (idC '-) (list (idC 'y) (numC 1))))))))

(check-equal? (parse '{/ f g} ) (appC (idC '/) (list (idC 'f) (idC 'g))))

(check-equal? (parse "test") (strC "test"))

(check-exn
 #px"ZODE: invalid symbol for an id"
 (λ () (parse 'if)))

(check-equal? (parse '{if : (<= 10 5) : (+ 10 5) : false})
              (ifC (appC (idC '<=) (list (numC 10) (numC 5))) (appC (idC '+) (list (numC 10) (numC 5))) (idC 'false)))

;;drew parse locals tests
(check-equal? (parse '{locals :  num x = 5 : {+ x 1}})
              (appC (lambC (list 'x) (list (numT)) #f (appC (idC '+) (list (idC 'x) (numC 1)))) (list (numC 5))))

(check-equal? (parse '{locals : num x = 5 : num y = 7 : {+ x y}})
              (appC (lambC (list 'x 'y) (list (numT) (numT)) #f (appC (idC '+) (list (idC 'x) (idC 'y)))) (list (numC 5) (numC 7))))

(check-equal? (parse '{locals : {num -> {num -> num}} f = {lamb : [num x] -> {num -> num} : {lamb : [num y] -> num : {+ x y}}} : {{f 3} 7}})
              (appC (lambC (list 'f) (list (funT (list (numT)) (funT (list (numT)) (numT)))) #f (appC (appC (idC 'f) (list (numC 3))) (list (numC 7))))
                    (list (lambC (list 'x) (list (numT)) (funT (list (numT)) (numT)) (lambC (list 'y) (list (numT)) (numT) (appC (idC '+) (list (idC 'x) (idC 'y))))))))

(check-exn
 #px"ZODE: Invalid clause syntax"
 (λ () (parse '{locals : x ?= 5 : {+ x 1}})))

(check-exn
 #px"ZODE: Invalid Identifier Name"
 (λ () (parse '{locals : num : = "" : "World"})))

(check-exn
 #px"ZODE: Invalid Identifier Name"
 (λ () (parse '{locals : num : = "" : num x = 5 : "World"})))


(check-exn
 #px"ZODE: Invalid Zode Syntax"
 (λ () (parse #t)))

;;parse two identical params to lamb
(check-exn
 #px"ZODE: Invalid Syntax or Params to lamb"
 (λ()(parse '{lamb : [num x] [num x] -> num : 10})))

;;test non symbol params to lamb
(check-exn
 #px"ZODE: Invalid parameter to lamb"
 (λ()(parse '{lamb :  [num 1] [num 2] [num x] [num y] -> num : {+ 2 3}})))

;;local with duplicate clauses
(check-exn
 #px"ZODE: Locals can't have duplicate clauses"
 (λ()(parse '{locals : num z = {lamb : : 3}
                     : num z = 9
                     : {z}})))

;;parse no body local--not the best solution for it...
(check-exn
 #px"ZODE: Locals must have a body expression"
 (λ()(parse '{locals : x = 5 :})))
(check-exn
 #px"ZODE: Invalid syntax for lamb"
 (λ()(parse '{lamb : i : "Hello" 31/7})))


;;PARSE TYPE TESTS-----------------------------------------------------------
;;test parse-type
(check-equal? (parse-type 'num) (numT))
(check-equal? (parse-type '{num str bool -> bool}) (funT (list (numT) (strT) (boolT)) (boolT)))
(check-exn
 #px"ZODE: Invalid Type Syntax"
 (λ() (parse-type 'apple)))

;;TYPE-CHECK TESTS
(check-equal? (type-check (idC '+) base-tenv) (funT (list (numT) (numT)) (numT)))
(check-equal? (type-check (idC 'true) base-tenv) (boolT))
(check-equal? (type-check (appC (idC '+) (list (numC 5) (numC 6))) base-tenv) (numT))
(check-equal? (type-check (appC (idC '+)
                                (list (numC 5) (appC (idC '-)
                                                     (list (numC 10) (numC 3)))))
                          base-tenv) (numT))
(check-equal? (type-check (ifC (appC (idC 'num-eq?) (list (numC 0) (numC 0)))
                               (numC 1)
                               (numC -1)) base-tenv) (numT))
(check-equal? (type-check (lambC (list 'x 'y) (list (numT) (numT))
                                    (boolT)
                                    (appC (idC 'num-eq?) (list (numC 0) (numC 1)))) base-tenv)
              (funT (list (numT) (numT)) (boolT)))
(check-exn
 #px"ZODE: lamb type mismatch"
 (λ () (type-check (lambC(list 'x 'y) (list (numT) (numT))
                                    (strT)
                                    (appC (idC 'num-eq?) (list (numC 0) (numC 1)))) base-tenv)))
(check-exn
 #px"ZODE: if condition not a boolean"
 (λ () (type-check (ifC (appC (idC 'substring) (list (strC "apple") (numC 1) (numC 3)))
                        (numC 1)
                        (numC -1)) base-tenv)))
(check-exn
 #px"ZODE: then and else statements do not match"
 (λ () (type-check (ifC (appC (idC 'num-eq?) (list (numC 0) (numC 0)))
                        (numC 1)
                        (idC 'false)) base-tenv)))
(check-exn
 #px"ZODE: name not found in type environment: 'banana"
 (λ () (type-check (idC 'banana) base-tenv)))
(check-exn
 #px"ZODE: non function type applied to arguments"
 (λ () (type-check (appC (numC 2) (list (numC 3) (numC 4))) base-tenv)))
(check-exn
 #px"ZODE: arguments not of compatible type to function"
 (λ () (type-check (appC (idC '+) (list (idC '-) (numC 5))) base-tenv)))


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
(check-equal? (interp (lambC (list 'x 'y 'z) (list (numT) (numT) (numT)) (numT)(numC 10)) '()) (cloV (list 'x 'y 'z) (numC 10) '()))

;;TEST SERIALIZE
(check-equal? (serialize (numV 10)) "10")
(check-equal? (serialize (cloV (list 'x 'y 'z) (appC (idC '+) (list (idC 'x) (numC 10))) '()))"#<procedure>")
(check-equal? (serialize (primV '+)) "#<primop>")
(check-equal? (serialize (strV "hello world")) "\"hello world\"")
(check-equal? (serialize (boolV 'true)) "true")
(check-equal? (serialize (boolV 'false)) "false")
(check-exn ;;tests not a bolean
 #px"Invalid boolean value 'notabool"
 (λ () (serialize (boolV 'notabool))))

;;TEST SERIALIZE2
(check-equal? (serialize2 (numV 10)) "10")
(check-equal? (serialize2 (cloV (list 'x 'y 'z) (appC (idC '+) (list (idC 'x) (numC 10))) '()))"#<procedure>")
(check-equal? (serialize2 (primV '+)) "#<primop>")
(check-equal? (serialize2 (strV "hello world")) "hello world")
(check-equal? (serialize2 (boolV 'true)) "true")
(check-equal? (serialize2 (boolV 'false)) "false")
(check-exn ;;tests not a bolean
 #px"Invalid boolean value 'notabool"
 (λ () (serialize2 (boolV 'notabool))))

#;(;;TEST TOP-INTERP---------------------------------------------------------------
(check-equal? (top-interp "hello world") "\"hello world\"")
(check-equal? (top-interp 10) "10")
(check-equal? (top-interp 'true) "true")
(check-equal? (top-interp '{<= 9 10}) "true")
(check-equal? (top-interp '{if : {str-eq? "nick" "nick"} : {/ 10 2} : {- 10 4}}) "5")
(check-equal? (top-interp '{if : {str-eq? "nick" "nick"} : {/ 10 2} : {- 10 4}}) "5")
(check-equal? (top-interp '{if : {str-eq? "nick" "drew"} : {/ 10 2} : {- 10 4}}) "6")
(check-exn
 #px"ZODE: expects exactly two operands"
 (λ () (top-interp '{<= 10 20 30})))
(check-exn
 #px"ZODE: user-error \"NOT GOOD\""
 (λ () (top-interp '{error "NOT GOOD"})))
(check-equal? (top-interp '{{lamb : x y : {+ x y}} 5 6}) "11")
(check-exn
 #px"ZODE : Invalid number of arguments in function call"
 (λ () (top-interp '{{lamb : x y : {+ x y}} 5 6 7})))
;;not valid application test
(check-exn
 #px"ZODE: 7 is not a valid application"
 (λ () (top-interp '{7 8 9})))
)
;;*******************************************************************************************
;;stopped here, this test below currently fails, type-check needs to be tweaked, then
;;few more top interp test, then can move on to adding bindings to boxes
;;then can add parse for local-rec, typecheck for local-rec, then finally interp for local-rec

;;test locals
(check-equal? (top-interp '{locals : {num num -> num} f = {lamb : [num x] [num y] -> num : {+ {* y y} {* x x}}}
                                   : num g = {+ 5 6}
                                   :{f g 1}}) "122")

;;check divide by zero
(check-exn
 #px"ZODE:Divide by zero undefined"
 (λ() (top-interp '{/ 10 0})))

;;+ as param, fails...why?
(check-equal? (top-interp '{{lamb : [num /] -> num : {* / /}} 5}) "25")

;;HELPER TESTS------------------------------------------------------------------

;;test has-dups?
(check-equal? (has-dups? (list 'a 'b 'c 'd)) #f)
(check-equal? (has-dups? (list 'a 'b 'c 'c)) #t)

;;test parse-clause-types
(check-equal? (parse-clause-types '{num x = {+ 5 6}}) (list (numT)))
(check-equal? (parse-clause-types '{num x = {+ 5 6} : num y = {- 6 5}}) (list (numT) (numT)))
(check-exn
 #px"ZODE: Invalid syntax in locals clause"
 (λ() (parse-clause-types '{x == : {+ 5 6}})))

;;test extend-env, flipped order, to allow for prim ops as variables
(check-equal? (extend-env (list 'x 'y 'z) (list (numV 1) (numV 2) (numV 3)) (list (Binding 'a (numV 10))))
              (list
               (Binding 'x (numV 1))
               (Binding 'y (numV 2))
               (Binding 'z (numV 3))
               (Binding 'a (numV 10))))

;;testing extend-tenv
(check-equal? (extend-tenv (list 'x) (list (numT)) (list (TBinding 'a (strT)))) (list (TBinding 'x (numT))
                                                                                      (TBinding 'a (strT))))
(check-equal? (type-check (numC 5) base-tenv) (numT))
(check-equal? (type-check (strC "fig") base-tenv) (strT))

;;test prim+-----------------------------------------
(check-equal? (prim+ (list (numV 1) (numV 2))) (numV 3))
(check-exn
 #px"ZODE: operands must be reals"
 (λ () (prim+ (list (numV 1) (strV "uhoh")))))
(check-exn
 #px"ZODE: expects exactly two operands"
 (λ () (prim+ (list (numV 1) (numV 2) (numV 3)))))

;;test prim-
(check-equal? (prim- (list (numV 1) (numV 2))) (numV -1))
(check-exn
 #px"ZODE: operands must be reals"
 (λ () (prim- (list (numV 1) (strV "uhoh")))))
(check-exn
 #px"ZODE: expects exactly two operands"
 (λ () (prim- (list (numV 1) (numV 2) (numV 3)))))

;;test prim*
(check-equal? (prim* (list (numV 10) (numV 2))) (numV 20))
(check-exn
 #px"ZODE: operands must be reals"
 (λ () (prim* (list (numV 1) (strV "uhoh")))))
(check-exn
 #px"ZODE: expects exactly two operands"
 (λ () (prim* (list (numV 1) (numV 2) (numV 3)))))

;;test prim/
(check-equal? (prim/ (list (numV 10) (numV 2))) (numV 5))
(check-exn
 #px"ZODE: operands must be reals"
 (λ () (prim/ (list (numV 1) (strV "uhoh")))))
(check-exn
 #px"ZODE: expects exactly two operands"
 (λ () (prim/ (list (numV 1) (numV 2) (numV 3)))))

;;test prim<=
(check-equal? (prim<= (list (numV 10) (numV 2))) (boolV 'false))
(check-equal? (prim<= (list (numV 5) (numV 10))) (boolV 'true))
(check-exn
 #px"ZODE: operands must be reals"
 (λ () (prim<= (list (numV 1) (strV "uhoh")))))
(check-exn
 #px"ZODE: expects exactly two operands"
 (λ () (prim<= (list (numV 1) (numV 2) (numV 3)))))

;;test prim-error
(check-exn
 #px"ZODE: user-error \"uh oh\""
 (λ ()(prim-error (list (strV "uh oh")))))
(check-exn
 #px"ZODE: error function takes only 1 input"
 (λ ()(prim-error (list (strV "uh oh") (strV "too many")))))

;;test prim-str-eq?
(check-exn
 #px"ZODE: str-eq\\? expects exactly two operands"
 (λ()(prim-str-eq? (list (strV "one") (strV "one") (strV "one")))))

;;test prim-num-eq?
(check-equal? (prim-num-eq? (list (numV 10) (numV 2))) (boolV 'false))
(check-exn
 #px"ZODE: num-eq\\? expects exactly two operands"
 (λ()(prim-num-eq? (list (numV 1) (numV 1) (numV 1)))))

;;test prim-substring
(check-equal? (prim-substring (list (strV "hello") (numV 1) (numV 3))) (strV "el"))
(check-exn
 #px"ZODE: substring"
 (λ()(prim-substring (list (strV "hello") (numV 10)))))


;test apply-prims
(check-equal? (apply-prims '+ (list (numV 1) (numV 2))) (numV 3))
(check-equal? (apply-prims '- (list (numV 1) (numV 2))) (numV -1))
(check-equal? (apply-prims '* (list (numV 1) (numV 2))) (numV 2))
(check-equal? (apply-prims '/ (list (numV 2) (numV 1))) (numV 2))
(check-equal? (apply-prims '<= (list (numV 1) (numV 2))) (boolV 'true))
(check-equal? (apply-prims 'num-eq? (list (numV 1) (numV 1))) (boolV 'true))
(check-equal? (apply-prims 'num-eq? (list (strV "one") (strV "one"))) (boolV 'true))
(check-equal? (apply-prims 'substring (list (strV "apple") (numV 1) (numV 3))) (strV "pp"))
(check-exn
 #px"ZODE: 'h is not a valid operator"
 (λ () (apply-prims 'h (list (numV 1) (numV 2)))))