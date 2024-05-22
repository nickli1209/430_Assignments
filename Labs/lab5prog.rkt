#lang racket

(module sim-ZODE4 racket
  (provide
   [rename-out (#%lam-app #%app)
               (my-if if)]
   else
   #%module-begin
   #%datum
   + - * / = equal? <= =>
   true false
   locals)
  (require (for-syntax syntax/parse))
 
  (define-syntax (#%lam-app stx)
    (syntax-case stx (lamb :)
      [(_ lamb : args ... : body)
       #'(lambda (args ...) body)]
      [(_ e ...)
       #'(#%app e ...)]))
 
  (define-syntax (my-if stx)
    (syntax-case stx (:)
      [(_ : e1 : e2 : e3)
       #'(if e1 e2 e3)]))
 
  (define-syntax (locals stx)
    (syntax-parse stx
      [((~literal locals)
        (~literal :)
        (~seq var:id (~literal =) rhs (~literal :)) ...
        body:expr)
       #'(let ([var rhs] ...) body)])))

(module my-module (submod ".." sim-ZODE4)
 
  {locals :
          one = {lamb : f : {lamb : a : {f a}}} :
          two = {lamb : f : {lamb : a : {f {f a}}}} :
          add = {lamb : n : {lamb : m : {lamb : f : {lamb : x : {{m f} {{n f} x}}}}}} :
          {{{{add one} two} {lamb : x : {* x 2}}} 3}})

(require 'my-module)