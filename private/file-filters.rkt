#lang racket
(require racket/gui)
(provide filter-dialog)
(define (filter-dialog dparent)
  (define filter-dialog%
    (class dialog%
      (super-new)
  (define default (get-preference 'files-viewer:filter-types))
  (define panel (new vertical-panel% [parent this]
                     [alignment '(left top)]))
  (define panel2 (new horizontal-panel% [parent this]
                      [alignment '(right bottom)]))
  (new message% [label "I want to ..."][parent panel])
  (define hide.files (new check-box% 
                          [parent panel]
                          [label "Hide dot prefix files and directories"]
                          [value (get-preference 'files-viewer:filter-types3)]))
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
                                                 files-viewer:filter-types2
                                                 files-viewer:filter-types3)
                                               (list (string-split (send types get-value) " ")
                                                     (= 1 (send choice get-selection))
                                                     (send hide.files get-value)
                                                     ))
                              (send this show #f))]))
  (define cancel (new button% [label "Cancel"] [parent panel2]
                      [callback (λ (c e)
                                  (send this show #f))]))
  (define/override (on-subwindow-char recv ev)
      (when (equal? (send ev get-key-code) #\return)
        (send ok command (make-object control-event% 'button (current-milliseconds))))
      (super on-subwindow-char recv ev))
  
  ))
  (define d (new filter-dialog% [width 600][label "File Filter"][parent dparent]))
  (send d show #t))