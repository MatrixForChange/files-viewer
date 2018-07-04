#lang racket
(provide (all-defined-out))
(define rkt-file-content "#lang racket")
(define rkt-gui-file-content
  "#lang racket/gui
(define frame (new frame% [label \"Racket Gui Program\"][width 400][height 300]))
(send frame show #t)")

(define rkt-macro-file-content
  "#lang racket
   (require (for-syntax racket racket/syntax syntax/parse syntax/parse/experimental/template
            syntax/transformer syntax/stx))")

(define markdown-file-content "")

