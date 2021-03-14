; The "hosted" langauge we're implementing our domain-specific language
; (DSL) in. In this case it's textbook-provided one with presumably helpful
; libraries for this purpose that either don't exist or are more complicated
; in pure Racket.
#lang br/quicklang

; All languages containe a function that converts syntax into S-expressions
; This is called read-syntax and it takes a path string and a
; file object (called ports here)
(define (read-syntax path port)
  ; Get back a list of all lines in the program. This program is small so we're
  ; not worried about memory issues
  (define src-lines (port->lines port))
  ; Wrap each line around a function call called 'handle' which we shall define
  ; It will have to be quoted
  (define src-datums (format-datums '(handle ~a) src-lines))
  ; Emit an racket module (akin to a compilation unit in other languages)
  ; that we've hardcoded for now to "stacker-mod" using the "expander"
  ; function we'll find in this very same file in this very same path
  ; Again, this will be unevaluated data where we've spliced our list
  ; of read/processed unevaluated s-expressions in as the body.
  (define module-datum `(module stacker-mod "stacker.rkt" ,@src-datums))
  ; Annotate that unevaluated module body with some runtime-helper
  ; context info.
  (datum->syntax #f module-datum))
; Make this reader available externally. Remember that this name is
; conventional
(provide read-syntax)

; This macro is our "expander", the function which goes and
; replaces those s-expressions from the reader with a generated
; program of the hosted language that will be computed on runtime
;
; It has to be called '#%module-begin but since we happen to be reusing
; our hosted-language's one we'll name it something unique firstand then
; "re-export" it out.
;
; It takes our body of s-expressions from the reader as input arguments
; the ... indicates we'll process this over the entire body.
(define-macro (stacker-module-begin HANDLE-EXPR ...)
  ; Generate a synrax object. This is a shorthand for datum->syntax above
  ; and it implicitly and passes through the current lexical context that
  ; in the reader we had explicitly set to having no context.
  ;
  ; In this case we're just evaluating our supplied s-expressions and then,
  ; at the end of execution, priting off the first value in our stack that
  ; in a proper RPN computer would be the value at the end of the program.
  ;
  ; Fun tip: quote the HANDLE-EXPR to see exactly what we're emitting
  ; rather than computing it
  #'(#%module-begin
     HANDLE-EXPR ...
     (display (first stack))))
; rename our expander and export it so racket may use it conventionally.
(provide (rename-out [stacker-module-begin #%module-begin]))

;; What follows are RPN runtime helpers

; Our stack starts off empty
(define stack empty)

(define (pop-stack!)
  (define arg (first stack))
  (set! stack (rest stack))
  arg)

(define (push-stack! arg)
  (set! stack (cons arg stack)))

; This is the bulk of ourevaluator; if we pass in a number, pop it on the stack
; if it's one of a fixed set of operations, pop and evaluate the last two args
; and push the result onto the stack.
(define (handle [arg #f])
  (cond
    [(number? arg) (push-stack! arg)]
    [(or (equal? + arg) (equal? * arg))
     (define op-result (arg (pop-stack!) (pop-stack!)))
     (push-stack! op-result)]))
(provide handle)

; Since we're just using the hosted language's math operations
; we need to re-export them here.
(provide + *)