#lang racket
(require racket/gui)
(provide filter-dialog)
(define (filter-dialog dparent)
  (define d (new dialog% [width 600][label "File Filter"][parent dparent]))
  (define default (get-preference 'files-viewer:filter-types))
  (define panel (new vertical-panel% [parent d]
                     [alignment '(left top)]))
  (define panel2 (new horizontal-panel% [parent d]
                      [alignment '(right bottom)]))
  (new message% [label "I want to ..."][parent panel])
  (define choice (new radio-box% [choices '("hide these files."
                                            "show these files.")]
                      [label ""]
                      [parent panel]
                      [selection (if (get-preference 'files-viewer:filter-types2)
                                     1 0)]))
  (new message% [label "Files Types(such as \".bak .zo\"):"][parent panel])                    
  (define types (new text-field% [label ""]
                     [parent panel][init-value (if default (string-join default " ")
                                               "")]))
  (define ok (new button% [label "OK"][parent panel2]
                  [callback (λ (c e)
                              (put-preferences '(files-viewer:filter-types
                                                 files-viewer:filter-types2)
                                               (list (string-split (send types get-value) " ")
                                                     (= 1 (send choice get-selection))
                                                     ))
                              (send d show #f))]))
  (define cancel (new button% [label "Cancel"] [parent panel2]
                      [callback (λ (c e)
                                  (send d show #f))]))
  
  (send d show #t))