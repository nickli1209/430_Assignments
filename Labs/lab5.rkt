#lang racket

;;Warmup
(define one (λ (f)
              (λ (a)
                (f a))))

#;((one -) 2)

;; two
(define two (λ (f)
              (λ (a)
                (f (f a)))))

#;((two -) 2)

;; zero
(define zero (λ (f)
               (λ (a)
                 a)))

#;((zero -) 3)

;;add1
;;takes in a number like function and returns a number like function which
;;does the function one more time
(define add1 (λ (n)
               (λ (f)
                 (λ (x)
                   (f ((n f) x))))))

#;(((add1 zero) -) 3)

;;add
(define add (λ (n)
              (λ (m)
                (λ (f)
                  (λ (x)
                    ((m f) ((n f) x))
                  )))))

#;((((add one) two) -) 4)

;;tru
(define tru (λ (a)
              (λ (b)
                a)))

#;((tru 5) 6)

;;fals
(define fals (λ (a)
               (λ (b)
                 b)))

#;((fals 7) 8)

;;if
(define if (λ (a)
             (λ (b)
               (λ (c)
                 ((a b) c)))))

#;(((if tru) 4) 5)
#;(((if fals) 4) 5)


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

;;7W9SVF
