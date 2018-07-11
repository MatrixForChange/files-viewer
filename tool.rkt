#lang racket
(require racket/gui drracket/tool
         framework
         "private/main.rkt"
         )
(provide tool@)


(define-unit tool@
  (import drracket:tool^)
  (export drracket:tool-exports^)
  (match-define (and phase1 phase2) void)
  
  (define drracket-frame-mixin
    (mixin (drracket:unit:frame<%> (class->interface drracket:unit:frame%)) ()
      
      (define main-directory (let ([main-dir (get-preference 'files-viewer:directory)])
                               (if (and main-dir (directory-exists? main-dir))
                                   main-dir
                                   #f)))
      (define is-show (get-preference 'files-viewer:is-show))
      (define auto-refresh? (get-preference 'files-viewer:auto-refresh))
      
      (define fschange (new fschange%))
      (define fschange-timer
        (new timer%
             [notify-callback
              (λ () (when (send fschange need-update?!)
                      (update-files!)))]
             [interval 1000]))
      
      
      (define *popup-menu #f)
      (define *files #f)
      (define *show/hide-plugin #f)
      (define (update-files!)
        (when (and main-directory (directory-exists? main-directory))
          (send *files set-dir! main-directory)
          (send *files update-files!)
          (update-fschange)))
      
      (define (change-to-directory dir)
        (let/ec exit
          (when dir
            (with-handlers ([exn:fail?
                             (λ (e)
                               (exit
                                (message-box "error" "can't open the directory")))])
              (directory-list
               dir))
            (set! main-directory dir)
            (put-preferences '(files-viewer:directory)
                             (list (~a dir)))
            (update-files!))))
      (unless main-directory (change-to-directory (find-system-path 'home-dir)))

      
      (inherit get-show-menu change-to-file change-to-tab create-new-tab
               get-current-tab open-in-new-tab find-matching-tab)

      (define/private (update-fschange)
        (when auto-refresh?
          (send fschange change-dirs (send *files get-opened-inside))))
      
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
                                 (change-to-directory (get-directory))
                                 )]
                               [refresh-callback (thunk (update-files!))]
                               [new-file-callback (thunk (define item (send *files get-selected))
                                                         (define p (if item (send item user-data) main-directory))
                                                         (new-file-dialog this p)
                                                         (update-files!))]
                               [delete-file-callback (thunk (define item (send *files get-selected))
                                                            (if item (begin
                                                                       (delete-file-and-directory this
                                                                                                  (send item user-data))
                                                                       (update-files!))
                                                                (message-box "error" "nothing to delete.")))]
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
                                                            )]
                               [open-terminal-here-callback
                                (thunk (define item (send *files get-selected))
                                       (define cmd (get-preference 'files-viewer:cmd))
                                       (define p (if item (send item user-data) main-directory))
                                       (define path (if (file-exists? p) (simplify-path
                                                                          (build-path p 'up))
                                                        p))
                                       (if cmd (process/safe (format cmd path))
                                           (message-box "error" "Command to Open Terminal Undefined")))]
                               [terminal-config-callback
                                (thunk (define cmd-config
                                         (new cmd-dialog% [parent this]))
                                       (send cmd-config show #t))]
                               [parent-directory-callback
                                (thunk (when main-directory
                                         (change-to-directory
                                          (simplify-path (build-path main-directory 'up)))))]
                               [change-to-this-directory-callback
                                (thunk (let/ec exit
                                         (define item (send *files get-selected))
                                         (unless item (exit (message-box "error" "nothing is selected")))
                                         (unless (directory-exists?
                                                  (send item user-data)) (exit (message-box "error" "not a directory")))
                                         (change-to-directory (send item user-data))))]
                               [auto-refresh-status auto-refresh?]
                               [auto-refresh-callback
                                (λ (v)
                                  (set! auto-refresh? v)
                                  (put-preferences '(files-viewer:auto-refresh) (list v))
                                  (if v
                                      (update-files!)
                                      (send fschange change-dirs '())))]
                               ))
        
          
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
                          [opened-change-callback (λ () (update-fschange))]
                          ))
        (update-files!)
        (unless is-show
          (send area change-children
                (λ (x)
                  (filter
                   (λ (x) (not (eq? real-area x))) x))))
        (make-object vertical-panel% area))
      

      (define/augment (on-close)
        (send fschange shutdown))
      (super-new)
      ))
  (drracket:get/extend:extend-unit-frame drracket-frame-mixin)
  
  )
