#lang typed/racket
(require typed/rackunit)
;;assingnment 3
;;not fully implemented


(define-type ExprC (U Real Variable BinopC))
(struct Binop ([op : Symbol] [left : ExprC] [right : ExprC])
  #:transparent)
(struct Variable ([name : Symbol])
  #:transparent)

(define (parse-expr [sexp : Sexp]) : ExprC
  (match sexp
    ;;<num>
    [(? real? n)    (NumC n)]
    ;;{+ <expr> <expr>}
    [(list '+ l r)     (PlusC (parse l) (parse r))]
    ;;{* <expr> <expr>
    [(list '* l r)     (MultC (parse l) (parse r))]
    ;;syntax error
    [other (error 'parse "invalid syntax. got: ~e" other)]))

(define (parse-fundef [sexp : Sexp]) : FundefC)

(define (parse-prog [sexp : Sexp]) : (Listof FundefC))


;;worry about later at intepr time
;; (define (op-table [b : Binop]) : ExprC
;;   (match b
;;     [(Binop '+ l r)      ....]
;;     [(Binop '- l r)      ....]
;;     [(Binop '* l r)      ....]
;;     [(Binop '/ l r)      ....]))

(define (parse-expr [sexp : Sexp]) : ExprC
  (match sexp
    [(? real? sexp) (NumC sexp)]))

(define (parse-fundef [sexp : Sexp]) : FundefC)

(define (parse-prog [sexp : Sexp]) : (Listof FundefC))
