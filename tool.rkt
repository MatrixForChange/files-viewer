#lang racket
(require racket/gui drracket/tool
         framework
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
  (define is-show #t)
  (define file-tab-recorder (make-hash))
  (define *change-directory #f)
  (define *files #f)
  (define *show-plugin #f)
  (define *hide-plugin #f)
  (define (update-files!)
    (when (and main-directory (directory-exists? main-directory))
      (send *files set-dir! main-directory)
      (send *files update-files!)))
  (define drracket-frame-mixin
    (mixin (drracket:unit:frame<%> (class->interface drracket:unit:frame%)) ()
      (super-new)
      (inherit get-show-menu change-to-file change-to-tab create-new-tab
               get-current-tab open-in-new-tab)
      (define/override (get-definitions/interactions-panel-parent)
        (define area (new my-horizontal-dragable% [parent (super get-definitions/interactions-panel-parent)]
                          ))
        (define real-area (new vertical-panel% [parent area]
                               ))
        (set! *change-directory (new button% [label "Change the Directory"]
                                     [parent real-area]
                                     [callback (lambda (b e)
                                                 (let/ec exit
                                                   (define dir (get-directory))
                                                   (with-handlers ([exn:fail?
                                                                    (λ (e)
                                                                      (message-box "error" "can't open the directory")
                                                                      (exit))])
                                                     (find-files (lambda (_) #t)
                                                                 dir))
                                                   (when dir
                                                     (set! main-directory dir)
                                                     (put-preferences '(files-viewer:directory)
                                                                      (list (path->string dir)))
                                                     (update-files!))))]
                                     ))
        
        (set! *show-plugin (new menu-item%
                                [label "Show the File Manager"]
                                [callback (lambda (c e) (unless is-show
                                                          (send area change-children
                                                                (lambda (x) (cons real-area x)))
                                                          (set! is-show #t))
                                            )]
                                [parent (get-show-menu)]
                                ))
        (set! *files (new directory-list% 
                          [parent real-area]
                          [select-callback (lambda (i)
                                             (let/ec exit
                                               (define ref (hash-ref file-tab-recorder i #f))
                                               (when ref (exit (change-to-tab ref))) 
                                               (when (send (get-current-tab) can-close?)
                                                 (exit (change-to-file i)))
                                               (open-in-new-tab i)
                                               (hash-set! file-tab-recorder i (get-current-tab))
                                               )
                                             )]))
        (set! *hide-plugin (new menu-item%
                                [label "Hide the File Manager"]
                                [callback (lambda (c e) (send area change-children
                                                              (λ (x)
                                                                (filter
                                                                 (λ (x) (not (eq? real-area x))) x)))
                                            (set! is-show #f))]
                                [parent (get-show-menu)]))
        (update-files!)
        (make-object vertical-panel% area))
      ))
  (drracket:get/extend:extend-unit-frame drracket-frame-mixin)
  
  )
