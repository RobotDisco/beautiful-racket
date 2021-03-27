; We've changed this to be a package, rather than needing our programs
; to be in them directory as our reader/parser/expander

#lang br/quicklang
(module reader br
  (require "reader.rkt")
  (provide read-syntax))