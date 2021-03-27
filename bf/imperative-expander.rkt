#lang br/quicklang
(define-macro (bf-module-begin PARSE-TREE)
  #'(#%module-begin
     PARSE-TREE))
(provide (rename-out [bf-module-begin #%module-begin]))

(define-macro (bf-program OP-OR-LOOP-ARG ...)
  #'(void OP-OR-LOOP-ARG ...)) ;; Don't print output, '.' will handle it.
(provide bf-program)

(define-macro (bf-loop "[" OP-OR-LOOP-ARG ... "]")
  #'(until (zero? (current-byte))
      OP-OR-LOOP-ARG ...))
(provide bf-loop)

(define-macro-cases bf-op
  [(bf-op ">") #'(gt)]        ; Increment byte position
  [(bf-op "<") #'(lt)]        ; Decrement byte position
  [(bf-op "+") #'(plus)]      ; increment current byte value
  [(bf-op "-") #'(minus)]     ; decrement current byte value
  [(bf-op ".") #'(period)]    ; output value of current byte
  [(bf-op ",") #'(comma)])    ; take value as input, store in current byte.
(provide bf-op)

(define arr (make-vector 30000 0)) ; bf uses 30k byte array as standard memory.
(define ptr 0) ; Start with 0th byte as the current one

(define (current-byte) (vector-ref arr ptr))
(define (set-current-byte! val) (vector-set! arr ptr val))

(define (gt) (set! ptr (add1 ptr)))
(define (lt) (set! ptr (sub1 ptr)))
(define (plus) (set-current-byte! (add1 (current-byte))))
(define (minus) (set-current-byte! (sub1 (current-byte))))
(define (period) (write-byte (current-byte)))
(define (comma) (set-current-byte! (read-byte)))

