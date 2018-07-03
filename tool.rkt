#lang racket
(require racket/gui drracket/tool
         "private/path-helpers.rkt"
         "private/gui-helpers.rkt")
(provide tool@)


(define-unit tool@
  (import drracket:tool^)
  (export drracket:tool-exports^)
  (define phase1 void)
  (define phase2 void)
  (define main-directory (if (get-preference 'files-viewer:directory)
                             (string->path (get-preference 'files-viewer:directory))
                             #f)
                             )
  (define *change-directory #f)
  (define *files #f)
  (define (update-files!)
    (when (and main-directory (directory-exists? main-directory))
      (send *files set-dir! main-directory)
      (send *files update-files!)))
  (define drracket-frame-mixin
    (mixin (drracket:unit:frame<%>) ()
      (super-new)
      (define/override (get-definitions/interactions-panel-parent)
        (define area (new horizontal-panel% [parent (super get-definitions/interactions-panel-parent)]
                         ))
        (define real-area (new vertical-panel% [parent area]
                               [stretchable-width #f]))
        (set! *change-directory (new button% [label "Change the directory"]
                                   [parent real-area]
                                   [callback (lambda (b e)
                                               (define dir (get-directory))
                                               (when dir
                                                 (set! main-directory dir)
                                                 (put-preferences '(files-viewer:directory)
                                                                  (list (path->string dir)))
                                                 (update-files!)))]
                                   [min-width 300]
                                   ))
        (set! *files (new directory-list% 
                          [parent real-area]
                          [select-callback (lambda (i)
                                      (send this change-to-file
                                       (send i user-data))
                                      )]))
        (update-files!)
        (make-object vertical-panel% area))
      ))
  (drracket:get/extend:extend-unit-frame drracket-frame-mixin)
  
  )
