#lang br/quicklang

(define-macro (bf-module-begin PARSE-TREE)
  #'(#%module-begin
     PARSE-TREE))
(provide (rename-out [bf-module-begin #%module-begin]))

(define (fold-funcs apl bf-funcs)
  ;; apl -> array/pointer list (how we represent bf memory state)
  ;; bf-funcs -> list of bf functions to apply
  ;;
  ;; from starting state, iterate through sequence of functions
  ;; and return final state.
  (for/fold ([current-apl apl])
            ([bf-func (in-list bf-funcs)])
    (apply bf-func current-apl)))

(define-macro (bf-program OP-OR-LOOP-ARG ...)
  #'(begin
      (define first-apl (list (make-vector 30000 0) 0))
      (void (fold-funcs first-apl (list OP-OR-LOOP-ARG ...)))))
(provide bf-program)

(define-macro (bf-loop "[" OP-OR-LOOP-ARG ... "]")
  #'(lambda (arr ptr)
      (for/fold ([current-apl (list arr ptr)])
                ; We need a sequence to overrate, so let's just
                ; iterate from 0 to infinity and use the #:break
                ; as a termination check.
                ([i (in-naturals)]
                 #:break (zero? (apply current-byte
                                       current-apl)))
        (fold-funcs current-apl (list OP-OR-LOOP-ARG ...)))))
(provide bf-loop)

(define-macro-cases bf-op
  [(bf-op ">") #'gt]
  [(bf-op "<") #'lt]
  [(bf-op "+") #'plus]
  [(bf-op "-") #'minus]
  [(bf-op ".") #'period]
  [(bf-op ",") #'comma])
(provide bf-op)

(define (current-byte arr ptr) (vector-ref arr ptr))

; Purely functional, copies the memory array and mutates the
; new one. This creates a lot of churn due to copying, memory creation,
; and garbage collection.
;
;(define (set-current-byte arr ptr val)
;  (define new-arr (vector-copy arr))
;  (vector-set! new-arr ptr val)
;  new-arr)

; We use a mutation here because nobody else
; uses the original variable and the cost
; of being purely functional isn't worth it.
(define (set-current-byte arr ptr val)
  (vector-set! arr ptr val)
  arr)

(define (gt arr ptr) (list arr (add1 ptr)))
(define (lt arr ptr) (list arr (sub1 ptr)))

(define (plus arr ptr)
  (list
   (set-current-byte arr ptr (add1 (current-byte arr ptr)))
   ptr))

(define (minus arr ptr)
  (list
   (set-current-byte arr ptr (sub1 (current-byte arr ptr)))
   ptr))

(define (period arr ptr)
  (write-byte (current-byte arr ptr))
  (list arr ptr))

(define (comma arr ptr)
  (list (set-current-byte arr ptr (read-byte)) ptr))



