; Convert conforming racket programs into RPN format.
; Useful for generating test input since we can conpute their answer
; in racket and then via the program we run.
#lang br/quicklang
; Specifies symbols in our language I guess
; (I guess numbers are taken for granted?)
(provide + *)

(define-macro (stackerizer-mb EXPR)
  #'(#%module-begin
     ; Take our macroized output, remove nesting,
     ; reverse it, and display each element on a new line
     (for-each displayln (reverse (flatten EXPR)))))
; Rename our custom macro to the standard expander convention
(provide (rename-out [stackerizer-mb #%module-begin]))

; A macro that takes a list of variadic math operations
; and produces them in associative / commutative dyadic
; forms.
; The ... means there will be an arbitrary N of them, and process
; them the same way.
(define-macro (define-ops OP ...)
  ; Unsure if this is basically progn or special macro syntax.
  ; As always, this is a syntax object so use #'
  #'(begin
      ; Macro in a macro! This uses pattern matching!
      (define-macro-cases OP
        ; First pattern, capture the last item in the s-expression.
        [(OP FIRST) #'FIRST]
        ; Recursive pattern, basically normalize argument list into
        ; a dyadic form and leave the remaining operations for the
        ; next recursive iteration.
        ; Note (... ...) is used to escape ellipses in the outer macro
        ; so the inner macro can handle them.
        [(OP FIRST NEXT (... ...))
         #'(list 'OP FIRST (OP NEXT (... ...)))])
      ...))

; Generate the macros that wind up defining our operator handlers.
(define-ops + *)
