; The s-exp says that there's no custom reader, these
; are regular racket s-expressions even if we must always
; have a custom expander.
#lang s-exp "stackerizer.rkt"
(* 1 2 (+ 3 4 (* 5 6 (+ 7 8 (* 9 10)))))
