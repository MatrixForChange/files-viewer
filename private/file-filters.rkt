#lang racket
(require racket/gui
         framework
         framework/preferences
         "gui-helpers.rkt")
(provide filter-dialog)
(define (filter-dialog dparent)
  (define filter-dialog%
    (class dialog%
      (super-new)
      (define default (preferences:get 'files-viewer:filter-types))
      (define panel (new vertical-panel% [parent this]
                         [alignment '(left top)]))
      (define panel2 (new horizontal-panel% [parent this]
                          [alignment '(right bottom)]))
      (new message% [label "I want to ..."][parent panel])
      (define hide.files (new check-box% 
                              [parent panel]
                              [label "Hide dot prefix files and directories."]
                              [value (preferences:get 'files-viewer:filter-types3)]))
      (define hide~files (new check-box% 
                              [parent panel]
                              [label (format "Hide backup files (ending in ~a)." (backup-file-suffix))]
                              [value (preferences:get 'files-viewer:filter-types4)]))
      (define choice (new radio-box% [choices '("Hide these files."
                                                "Show these files.")]
                          [label ""]
                          [parent panel]
                          [selection (if (preferences:get 'files-viewer:filter-types2)
                                         1 0)]))
      (new message% [label "Files types (such as \".dep .zo\"):"] [parent panel])
      (define types (new text-field%
                         [label ""]
                         [parent panel]
                         [init-value (if default (string-join default " ") "")]))
      (define cancel (new button%
                          [label "Cancel"]
                          [parent panel2]
                          [callback (λ (c e) (send this show #f))]))
      (define ok (new button%
                      [label "OK"]
                      [parent panel2]
                      [callback (λ (c e)
                                  (preferences:set 'files-viewer:filter-types (string-split (send types get-value) " "))
                                  (preferences:set 'files-viewer:filter-types2 (= 1 (send choice get-selection)))
                                  (preferences:set 'files-viewer:filter-types3 (send hide.files get-value))
                                  (preferences:set 'files-viewer:filter-types4 (send hide~files get-value))
                                  (send this show #f))]))
      
      (send types focus)
      (define/override (on-subwindow-char recv ev)
        (when (equal? (send ev get-key-code) #\return)
          (send ok command (make-object control-event% 'button (current-milliseconds))))
        (super on-subwindow-char recv ev))
  
      ))
  (define d (new filter-dialog% [width 600] [label "File filter"] [parent dparent]))
  (send d show #t))
