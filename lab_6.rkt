;;LAB 6
#lang typed/racket

;;ExprC from Assignment 5 here
(define example-program
  '{seq {println "Welcome to the reverse calculator game!"}
        {println "using a combination of the operations:"}
        {println "A. Divide by 2"}
        {println "B. Multiply by 3"}
        {println "C. add 1"}
        {println "D. subtract 1"}
        {println "Your goal is to get from the start number to the target number"}
        {println "in as few moves as possible. Good luck!"}
        {locals : start = {seq {println "Enter the starting number"}
                               {read-num}}
                : target = {seq {println "Enter the target number"}
                                {read-num}}
                ;;main game loop, recursion self passing
                : {locals : game = {lamb : self current target move
                                         : {if : {equal? current target}
                                               : {seq {println "Congrats! You won on move:"}
                                                      {printint move}}
                                               : {locals : op = {seq {println "-------------------------"}
                                                                     {println "CURRENT MOVE:"}
                                                                     {printint move}
                                                                     {println "Current number:"}
                                                                     {printint current}
                                                                     {println "Target number:"}
                                                                     {printint target}
                                                                     {println "Choose operation:"}
                                                                     {println "A:(Divide by 2)"}
                                                                     {println "B:(Mult by 3)"}
                                                                     {println "C:(Add 1)"}
                                                                     {println "D:(Subtract 1)"}
                                                                     {read-str}}
                                                         : {if : {equal? op "A"}
                                                               : {self self {/ current 2} target {+ 1 move}}
                                                               :{if : {equal? op "B"}
                                                                    : {self self {* 3 current} target {+ 1 move}}
                                                                    : {if : {equal? op "C"}
                                                                          : {self self {+ 1 current} target {+ 1 move}}
                                                                          :
                                                                          {if
                                                                           : {equal? op "D"}
                                                                           : {self self {- current 1} target {+ 1 move}}
                                                                           : {seq
                                                                              {println "Invalid Operator!"}
                                                                              {self self current target move}}}}}}}}}
                          : {game game start target 0}}}})

;;random-symbol function
;;returns one random symbol from a set of 8 selected symbols
(define syms '(a b c d e x y z))

(define (random-symbol)
  (list-ref syms (random 8)))






