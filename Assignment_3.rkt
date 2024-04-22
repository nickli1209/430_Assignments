#lang typed/racket
(require typed/rackunit)
;;assingnment 3
;;not fully implemented


(define-type ExprC (U Real Variable BinopC))
(struct Binop ([op : Symbol] [left : ExprC] [right : ExprC])
  #:transparent)
(struct Variable ([name : Symbol])
  #:transparent)

;; (define (op-table [b : Binop]) : ExprC
;;   (match b
;;     [(Binop '+ l r)      ....]
;;     [(Binop '- l r)      ....]
;;     [(Binop '* l r)      ....]
;;     [(Binop '/ l r)      ....]))

(define (parse-expr [sexp : Sexp]) : ExprC
  (match sexp
    [()]))

(define (parse-fundef [sexp : Sexp]) : FundefC)

(define (parse-prog [sexp : Sexp]) : (Listof FundefC))