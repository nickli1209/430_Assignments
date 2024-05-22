#lang typed/racket
(require typed/rackunit)
;;ASSIGNMENT 4, NICK LI DREW KIM
;;Should be fully implemented, ironing out kinks,
;;and syntax differences... 2 tests failed.. 

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
                      (Binding 'false (boolV 'false))
                      (Binding 'println (primV 'println))
                      (Binding 'read-num (primV 'read-num))
                      (Binding 'read-str (primV 'read-str))
                      (Binding 'seq (primV 'seq))
                      (Binding '++ (primV '++))
                      (Binding 'printint (primV 'printint))))


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
    [(list 'lamb ': params ... ': body)         (define syms (filter symbol? params))
                                                 (if(equal? (length syms) (length params))
                                                    (if (has-dups? syms)
                                                        (error 'parse "ZODE: Functions can't have duplicate params")
                                                        (lambC syms (parse body)))
                                                  (error 'parse"ZODE: Function contains non-symbol param"))]
    ;;need to make sure id's in locals are valid using allowed? helper
    [(list 'locals ': clause ... ': ex)        (if (has-dups? (parse-clause-ids (cast clause (Listof Sexp))))
                                                  (error 'parse"ZODE: Locals can't have duplicate clauses")
                                                  (appC
                                                   (lambC
                                                    (parse-clause-ids (cast clause (Listof Sexp)))
                                                    (parse ex))
                                                   (parse-clause-vals (cast clause (Listof Sexp)))))]
    ;; probably not the best solution for invaid lamb and local but def easiest
    [(list 'locals ': clause ... ':)           (error 'parse"ZODE: Locals must have a body expression")]
    [(list 'lamb ': _ ...)                            (error 'parse"ZODE: Invalid syntax for lamb")]
    ;;maybe need some error checking here for appC parse, 
    [(? list? aps)                    (appC (parse (first aps)) (map parse (rest aps)))]
    [else                         (error 'parse "ZODE: Invalid Zode Syntax")]))

;;expected exception with message containing ZODE on test expression: '(parse '(locals : : = "" : "World"))

;;PARSE LOCALS-------------------------------------
;;helper parse function, parses a clause and returns the list of ids
(define (parse-clause-ids [clauses : Sexp]) : (Listof Symbol)
  (match clauses
    ;;single clause case
    [(list (? symbol? id) '= _)        (if
                                         (allowed? id)
                                         (cons id '())
                                         (error 'invalid-id "ZODE: Invalid Identifier Name"))]
    ;;multiple clauses
    [(list (? symbol? id) '= _ ': more-clauses ...)       (if (allowed? id)
                                                            (cons id (parse-clause-ids more-clauses))
                                                            (error 'invalid-id "ZODE: Invalid Identifier Name"))]
    [else (error 'clause-syn "ZODE: Invalid clause syntax")]))

;;helper parse function, parses a clause and returns the list of vals
(define (parse-clause-vals [clauses : Sexp]) : (Listof ExprC)
  (match clauses
    [(list _ '= val)                            (cons (parse val) '())]
    [(list _ '= val ': more-clauses ...)            (cons (parse val) (parse-clause-vals more-clauses))]))

 
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
    [(lambC params body)   (cloV params body env)] ;;primV are created by bindings in top-env
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


;;prim-println
(define (prim-println [args : (Listof Value)]): boolV
  (match args
    [(list (strV s)) (println s) (boolV 'true)]
    [else (error'interp"ZODE: Invalid input to println")]))

;;prim-printint
(define (prim-printint [args : (Listof Value)]) : boolV
  (match args
    [(list (numV n)) (println (~v n)) (boolV 'true)]
    [else (error'interp"ZODE: Invalid input to printint")]))


;;prim-seq
(define (prim-seq [args : (Listof Value)]) : Value
  (if (not (empty? args))
      (last args)
      (error 'interp "ZODE: seq expects at least one arguement")))


;;prim-equal?
(define (prim-equal? [args : (Listof Value)]): Value
  (if (equal? (length args) 2)
      (let ([a (first args)]
            [b (second args)])
        (if (equal?  a  b)
            (boolV 'true)
            (boolV 'false))) 
      (error 'interp "ZODE: expects exactly two operands")))

;;prim-make-array
;;(define (make-array [size : Real] [init : Real]))

;;prim-array

;;prim-aref

;;prim-aset!

;;prim-substring





;;apply-prims, takes as input a Symbol op, and a list of values args
;;applys the appropriate primative operation and returns a Value
(define (apply-prims [op : Symbol] [args : (Listof Value)]): Value
  (match op
    ['+         (prim+ args)]
    ['*         (prim* args)]
    ['-         (prim- args)]
    ['/         (prim/ args)]
    ['<=         (prim<= args)]
    ['equal?     (prim-equal? args)]
    ['println    (prim-println args)]
    ['seq        (prim-seq args)]
    ['printint   (prim-printint args)]
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
(check-equal? (parse '{lamb : x y : {+ x 5}}) (lambC (list 'x 'y) (appC (idC '+) (list (idC 'x) (numC 5)))))
(check-equal? (parse '{lamb : x : {+ x {lamb : y : {- y 1}}}})
              (lambC (list 'x) (appC (idC '+) (list (idC 'x)
                                                    (lambC (list 'y) (appC (idC '-) (list (idC 'y) (numC 1))))))))
(check-equal? (parse '{/ f g} ) (appC (idC '/) (list (idC 'f) (idC 'g))))
(check-equal? (parse "test") (strC "test"))
(check-exn
 #px"ZODE: invalid symbol for an id"
 (λ () (parse 'if)))
(check-equal? (parse '{if : (<= 10 5) : (+ 10 5) : false})
              (ifC (appC (idC '<=) (list (numC 10) (numC 5))) (appC (idC '+) (list (numC 10) (numC 5))) (idC 'false)))
;;drew parse locals tests
(check-equal? (parse '{locals : x = 5 : {+ x 1}})
              (appC (lambC (list 'x) (appC (idC '+) (list (idC 'x) (numC 1)))) (list (numC 5))))
(check-equal? (parse '{locals : x = 5 : y = 7 : {+ x y}})
              (appC (lambC (list 'x 'y) (appC (idC '+) (list (idC 'x) (idC 'y)))) (list (numC 5) (numC 7))))
(check-equal? (parse '{locals : f = {lamb : x : {lamb : y : {+ x y}}} : {{f 3} 7}})
              (appC (lambC (list 'f) (appC (appC (idC 'f) (list (numC 3))) (list (numC 7))))
                    (list (lambC (list 'x) (lambC (list 'y) (appC (idC '+) (list (idC 'x) (idC 'y))))))))
(check-exn
 #px"ZODE: Invalid clause syntax"
 (λ () (parse '{locals : x ?= 5 : {+ x 1}})))
(check-exn
 #px"ZODE: Invalid Identifier Name"
 (λ () (parse '{locals : : = "" : "World"})))
(check-exn
 #px"ZODE: Invalid Identifier Name"
 (λ () (parse '{locals : : = "" : x = 5 : "World"})))

;;parse invalid syntax
#;(check-exn
 #px"ZODE: Invalid Zode Syntax"
 (λ () (parse (list 3 '& 5))));;with new def of appC, this should get caught as an appC
;;will not error here, will eror in iterp when no value for appC named 3 in env
(check-exn
 #px"ZODE: Invalid Zode Syntax"
 (λ () (parse #t)))

;;parse two identical params to lamb
(check-exn
 #px"ZODE: Functions can't have duplicate params"
 (λ()(parse '{lamb : x x : 10})))

;;test non symbol params to lamb
(check-exn
 #px"ZODE: Function contains non-symbol param"
 (λ()(parse '{lamb :  1 2 x y : {+ 2 3}})))

;;local with duplicate clauses
(check-exn
 #px"ZODE: Locals can't have duplicate clauses"
 (λ()(parse '{locals : z = {lamb : : 3}
                     : z = 9
                     : {z}})))

;;parse no body local--not the best solution for it...
(check-exn
 #px"ZODE: Locals must have a body expression"
 (λ()(parse '{locals : x = 5 :})))
(check-exn
 #px"ZODE: Invalid syntax for lamb"
 (λ()(parse '{lamb : i : "Hello" 31/7})))


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

;;TEST TOP-INTERP---------------------------------------------------------------
(check-equal? (top-interp "hello world") "\"hello world\"")
(check-equal? (top-interp 10) "10")
(check-equal? (top-interp 'true) "true")
(check-equal? (top-interp '{<= 9 10}) "true")
(check-equal? (top-interp '{if : {equal? "nick" "nick"} : {/ 10 2} : {- 10 4}}) "5")
(check-equal? (top-interp '{if : {equal? "nick" "nick"} : {/ 10 2} : {- 10 4}}) "5")
(check-equal? (top-interp '{if : {equal? "nick" "drew"} : {/ 10 2} : {- 10 4}}) "6")
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
;;test locals
(check-equal? (top-interp '{locals : f = {lamb : x y : {+ {* y y} {* x x}}}
                                   : g = {+ 5 6}
                                   :{f g 1}}) "122")
;;check divide by zero
(check-exn
 #px"ZODE:Divide by zero undefined"
 (λ() (top-interp '{/ 10 0})))

;;+ as param, fails...why?
(check-equal? (top-interp '{{lamb : / : {* / /}} 5}) "25")

;;test println
(check-equal? (top-interp '{println "hello"}) "true")

;;tets printint
(check-equal? (top-interp '{printint 10}) "true")

;;test seq
(check-equal? (top-interp '{seq {+ 10 20} {println "HELLO"} {+ 10 5}}) "15")

;;HELPER TESTS------------------------------------------------------------------

;;test has-dups?
(check-equal? (has-dups? (list 'a 'b 'c 'd)) #f)
(check-equal? (has-dups? (list 'a 'b 'c 'c)) #t)

;;test extend-env, flipped order, to allow for prim ops as variables
(check-equal? (extend-env (list 'x 'y 'z) (list (numV 1) (numV 2) (numV 3)) (list (Binding 'a (numV 10))))
              (list
               (Binding 'x (numV 1))
               (Binding 'y (numV 2))
               (Binding 'z (numV 3))
               (Binding 'a (numV 10))))

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

;;test prim-equal?
(check-equal? (prim-equal? (list (numV 10) (strV "hi"))) (boolV 'false))
(check-equal? (prim-equal? (list (strV "hi") (strV "hi"))) (boolV 'true))
(check-exn
 #px"ZODE: expects exactly two operands"
 (λ () (prim-equal? (list (numV 1) (numV 2) (numV 3)))))

;;test prim-error
(check-exn
 #px"ZODE: user-error \"uh oh\""
 (λ ()(prim-error (list (strV "uh oh")))))
(check-exn
 #px"ZODE: error function takes only 1 input"
 (λ ()(prim-error (list (strV "uh oh") (strV "too many")))))


;;test prim-println
(check-equal? (prim-println (list (strV "hello"))) (boolV 'true))
(check-exn
 #px"ZODE: Invalid input to println"
 (λ() (prim-println (list (numV 10)))))

;;test prim-printint
(check-equal? (prim-printint (list (numV 10))) (boolV 'true))
(check-exn
 #px"ZODE: Invalid input to printint"
 (λ() (prim-printint (list (strV "bjdndjfn")))))

;;test prim-seq
(check-equal? (prim-seq (list (numV 8) (numV 10))) (numV 10))
(check-exn
 #px"ZODE: seq expects at least one arguement"
 (λ()(prim-seq '())))

;test apply-prims
(check-equal? (apply-prims '+ (list (numV 1) (numV 2))) (numV 3))
(check-equal? (apply-prims '- (list (numV 1) (numV 2))) (numV -1))
(check-equal? (apply-prims '* (list (numV 1) (numV 2))) (numV 2))
(check-equal? (apply-prims '/ (list (numV 2) (numV 1))) (numV 2))
(check-equal? (apply-prims '<= (list (numV 1) (numV 2))) (boolV 'true))
(check-equal? (apply-prims 'equal? (list (numV 1) (numV 1))) (boolV 'true))
(check-exn
 #px"ZODE: 'h is not a valid operator"
 (λ () (apply-prims 'h (list (numV 1) (numV 2)))))



