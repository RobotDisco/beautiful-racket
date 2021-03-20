; This is basically a iteration on the "stacker" RPN DSL that doesn't 
; mutate the stack but uses pure functions instead. Differences from
; stacker will be noted.
#lang br/quicklang

(define (read-syntax path port)
  (define src-lines (port->lines port))
  (define src-datums (format-datums '~a src-lines))
  (define module-datum `(module funstacker-mod "funstacker.rkt"
                          ; We pass all lines of the program in as arguments
                          ; to one function which will process everything in
                          ; one go.
                          (handle-args ,@src-datums)))
  (datum->syntax #f module-datum))
(provide read-syntax)

(define-macro (funstacker-module-begin HANDLE-ARGS-EXPR)
  #'(#%module-begin
     ; Since the reader basically collapses everything to one function
     ; we just have to display the first value of the stack it has
     ; collapsed into one element.
     (display (first HANDLE-ARGS-EXPR))))
(provide (rename-out [funstacker-module-begin #%module-begin]))

; a . indicates that args should collect all remaining arguments
(define (handle-args . args)
  (for/fold (; accumulator starts with an empty list
             [stack-acc empty])
            ; arg just has to be a list but in-list optimizes the program
            ; to trust that this is in fact a list.
            ([arg (in-list args)]
             ; a gate to not trip up on void values
             #:unless (void? arg))
    (cond
      ; basically the same logic as before; just stack numbers, and
      ; replace the operator and operand values with the result value
      ; if we encounter a + or *
      ;
      ; the accumulator is basically the stack at that point in the fold.
      [(number? arg) (cons arg stack-acc)]
      [(or (equal? * arg) (equal? + arg))
       (define op-result
         (arg (first stack-acc) (second stack-acc)))
       (cons op-result (drop stack-acc 2))])))
(provide handle-args)

(provide + *)