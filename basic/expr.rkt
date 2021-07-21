#lang br
(provide b-sum b-expr)

(define (b-sum . vals)
  (if (= 1 (length vals))
      (car vals)
      (apply + vals)))

(define (b-expr expr)
  (if (integer? expr) (inexact->exact expr) expr))
