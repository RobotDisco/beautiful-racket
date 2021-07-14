#lang br/quicklang

;; module+ takes the parent module and builds a new submodule on top of it.
(module+ reader
  (provide read-syntax))

(define (read-syntax path port)
  (define wire-datums
    (for/list ([wire-str (in-lines port)])
      (format-datum '(wire ~a) wire-str)))
  ;; #` and #, and #,@ are like quasiquoting with syntax datums.
  ;; but also strip-bindings removes metadata macros preserve but read-syntax
  ;; cannot allow.
  (strip-bindings
   #`(module wires-mod wires/main
       #,@wire-datums)))

;; We're just going to pass everything through #%module-begin, so just use the
;; built-in one instead of building a no-op one.
(provide #%module-begin)

;; notice the recursion in cases 2 & 3; we have to keep in mind the possibility
;; of infinite loops in this recursion.
(define-macro-cases wire
  [(wire ARG -> ID)
   #'(define/display (ID) (val ARG))]
  [(wire OP ARG -> ID)
   #'(wire (OP (val ARG)) -> ID)]
  [(wire ARG1 OP ARG2 -> ID)
   #'(wire (OP (val ARG1) (val ARG2)) -> ID)]
  [else #'(void)])
(provide wire)

(define-macro (define/display (ID) BODY)
  #'(begin
      (define (ID) BODY)
      ;; the 'main' module always runs after the rest of the code has been
      ;; loaded, so we'll take advantage of this to ensure all of our wires
      ;; have finished being evaluated.
      (module+ main
        (displayln (format "~a: ~a" 'ID (ID))))))

;; the let-over-lambda leverages closures for effectively "private" data.
(define val
  (let ([val-cache (make-hash)])
    (lambda (num-or-wire)
      (if (number? num-or-wire)
          num-or-wire
          (hash-ref! val-cache num-or-wire num-or-wire)))))

(define (mod-16bit x) (modulo x 65536))
(define-macro (define-16bit ID PROC-ID)
  ;; compose is a higher-order-function version of writing a lambda that
  ;; explicitly composes two functions
  #'(define ID (compose1 mod-16bit PROC-ID)))

(define-16bit AND bitwise-and)
(define-16bit OR bitwise-ior)
(define-16bit NOT bitwise-not)
(define-16bit LSHIFT arithmetic-shift)
(define (RSHIFT x y) (LSHIFT x (- y)))
(provide AND OR NOT LSHIFT RSHIFT)
      