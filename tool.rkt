#lang racket
(require racket/gui drracket/tool
         framework
         "private/path-helpers.rkt"
         "private/gui-helpers.rkt"
         "private/popup-menu.rkt"
         "private/file-filters.rkt"
         "private/rename-dialog.rkt"
         )
(provide tool@)


(define-unit tool@
  (import drracket:tool^)
  (export drracket:tool-exports^)
  (define phase1 void)
  (define phase2 void)
  (define main-directory (let ()
                           (define main-dir (get-preference 'files-viewer:directory))
                           (if (and main-dir (directory-exists? main-dir))
                               main-dir
                               #f))
    )
  (define is-show (get-preference 'files-viewer:is-show))
  (define *popup-menu #f)
  (define *files #f)
  (define *show/hide-plugin #f)
  (define (update-files!)
    (when (and main-directory (directory-exists? main-directory))
      (send *files set-dir! main-directory)
      (send *files update-files!)))
  (define drracket-frame-mixin
    (mixin (drracket:unit:frame<%> (class->interface drracket:unit:frame%)) ()
      (super-new)
      (inherit get-show-menu change-to-file change-to-tab create-new-tab
               get-current-tab open-in-new-tab find-matching-tab)
      (define/override (get-definitions/interactions-panel-parent)
        (define area (new my-horizontal-dragable% [parent (super get-definitions/interactions-panel-parent)]
                          ))
        (define real-area (new vertical-panel% [parent area]
                               ))
        (set! *show/hide-plugin (new menu-item%
                                     [label (if is-show "Hide the File Manager" "Show the File Manager")]
                                     [callback (lambda (c e) (define is-show
                                                               (get-preference 'files-viewer:is-show))
                                                 (if is-show
                                                     (let () (send area change-children
                                                                   (λ (x)
                                                                     (filter
                                                                      (λ (x) (not (eq? real-area x))) x)))
                                                       (put-preferences '(files-viewer:is-show) '(#f))
                                                       (send c set-label "Show the File Manager"))
                                                     (let ()
                                                       (send area change-children
                                                             (lambda (x) (cons real-area x)))
                                                       (put-preferences '(files-viewer:is-show) '(#t))
                                                       (send c set-label "Hide the File Manager")))
                                                 )]
                                     [parent (get-show-menu)]
                                     ))
        (set! *popup-menu (new files-popup-menu%
                               [change-the-directory-callback
                                (thunk
                                 (let/ec exit
                                   (define dir (get-directory))
                                   (when dir
                                     (with-handlers ([exn:fail?
                                                      (λ (e)
                                                        (exit
                                                         (message-box "error" "can't open the directory")))])
                                       (directory-list
                                                   dir))
                                     (set! main-directory dir)
                                     (put-preferences '(files-viewer:directory)
                                                      (list (path->string dir)))
                                     (update-files!))))]
                               [refresh-callback (thunk (update-files!))]
                               [new-file-callback (thunk (define item (send *files get-selected))
                                                         (define p (if item (send item user-data) main-directory))
                                                         (new-file-dialog this p)
                                                         (update-files!))]
                               [delete-file-callback (thunk (define item (send *files get-selected))
                                                            (if item (begin
                                                                       (delete-file-and-not-directory
                                                                        (send item user-data))
                                                                       (update-files!))
                                                                (message-box "error" "no file to delete.")))]
                               [file-filter-callback (thunk (filter-dialog this)
                                                            (update-files!))]
                               [rename-file-callback (thunk (define item (send *files get-selected))
                                                            (if item (let ()
                                                                       (define rd (new rename-dialog%
                                                                                       [parent this]
                                                                                       [path (send item user-data)]))
                                                                       (send rd show #t)
                                                                       (update-files!))
                                                                (message-box "error" "no file or directory to rename."))
                               )]))
        
          
        (set! *files (new directory-list% 
                          [parent real-area]
                          [select-callback (lambda (i)
                                             (when (file-exists? i)
                                               (cond
                                                 [(find-matching-tab i) => change-to-tab]
                                                 [(not (send (send (get-current-tab)  get-defs) is-modified?)) (change-to-file i)]
                                                 [else (open-in-new-tab i)]))
                                             )]
                          [my-popup-menu *popup-menu]
                          ))
        (update-files!)
        (unless is-show
          (send area change-children
                (λ (x)
                  (filter
                   (λ (x) (not (eq? real-area x))) x))))
        (make-object vertical-panel% area))
      ))
  (drracket:get/extend:extend-unit-frame drracket-frame-mixin)
  
  )
