#lang racket
(require racket/gui)
(provide filter-dialog)
(define (filter-dialog dparent)
  (define d (new dialog% [width 600][label "File Filter"][parent dparent]))
  (define default (get-preference 'files-viewer:filter-types))
  (define types (new text-field% [label "Files Types(such as \".bak .zo\"):"]
                     [parent d][init-value (if default (string-join default " ")
                                               "")]))
  (define ok (new button% [label "OK"][parent d]
                  [callback (λ (c e)
                              (put-preferences '(files-viewer:filter-types)
                                               (list (string-split (send types get-value) " ")))
                              (send d show #f))]))
  (send d show #t))