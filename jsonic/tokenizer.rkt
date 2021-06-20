#lang br/quicklang
(require brag/support)

(define (make-tokenizer port)
  (define (next-token)
    (define jsonic-lexer
      (lexer
       ;; Handle comment lines
       [(from/to "//" "\n") (next-token)]
       ;; jsonic s-expression in json
       [(from/to "@$" "$@")
        (token 'SEXP-TOK (trim-ends "@$" lexeme "$@"))]
       ;; match anything else
       [any-char (token 'CHAR-TOK lexeme)]))
    (jsonic-lexer port))
  next-token)
(provide make-tokenizer)