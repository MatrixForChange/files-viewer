#lang racket
(require racket/gui drracket/tool
         framework
         "private/main.rkt"
         )
(provide tool@)


(define-unit tool@
  (import drracket:tool^)
  (export drracket:tool-exports^)
  (define phase1 (thunk
                  (preferences:set-default 'files-viewer:directory
                                           (find-system-path 'home-dir)
                                           (λ (x) (and x (directory-exists? x))))
                  (preferences:set-default 'files-viewer:behavior-open
                                           2
                                           (λ (x) (member x '(0 1 2))))
                  (preferences:set-default 'files-viewer:binary-file-open
                                           #f
                                           boolean?)
                  (preferences:set-default 'files-viewer:is-show
                                           #t
                                           boolean?)
                  (preferences:set-default 'files-viewer:filter-types3
                                           #t
                                           boolean?)
                  (preferences:set-default 'files-viewer:filter-types2
                                           #f
                                           boolean?)
                  (preferences:set-default 'files-viewer:filter-types
                                           '()
                                           list?)
                  (preferences:set-default 'files-viewer:percentages
                                           (list #e0.15 #e0.85)
                                           list?)
                  (preferences:set-default 'files-viewer:auto-refresh
                                           #f
                                           boolean?)
                  (preferences:set-default 'files-viewer:cmd
                                           (match (system-type 'os)
                                             ['windows "start /d \"~a\" cmd"]
                                             ['macosx (if (directory-exists? "/System/Applications/Utilities/Terminal.app")
                                                          "open -a /System/Applications/Utilities/Terminal.app '~a'"
                                                          "open -a /Applications/Utilities/Terminal.app '~a'")]
                                             [_ ""])
                                           string?)
                  (preferences:set-default 'files-viewer:workspaces
                                           '()
                                           list?)
                  ))
  (define phase2 void)
  
  (define drracket-frame-mixin
    (mixin (drracket:unit:frame<%> (class->interface drracket:unit:frame%)) ()
      
      (define main-directory (preferences:get 'files-viewer:directory))
      (define is-show (preferences:get 'files-viewer:is-show))
      (define auto-refresh? (preferences:get 'files-viewer:auto-refresh))
      
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
      (define *dir-control #f)
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
                                (message-box "Error" "Can't open the directory")))])
              (directory-list
               dir))
            (set! main-directory dir)
            (preferences:set 'files-viewer:directory
                             (~a dir))
            (send *dir-control set-path (path-alist dir))
            (update-files!))))

      
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
                                                               (preferences:get 'files-viewer:is-show))
                                                 (if is-show
                                                     (let () (send area change-children
                                                                   (λ (x)
                                                                     (filter
                                                                      (λ (x) (not (eq? real-area x))) x)))
                                                       (preferences:set 'files-viewer:is-show #f)
                                                       (send c set-label "Show the File Manager"))
                                                     (let ()
                                                       (send area change-children
                                                             (lambda (x) (cons real-area x)))
                                                       (preferences:set 'files-viewer:is-show #t)
                                                       (send c set-label "Hide the File Manager")))
                                                 )]
                                     [parent (get-show-menu)]
                                     ))
        (set! *dir-control (new dir-control%
                                [parent real-area]
                                [highlighted "lightskyblue"]
                                [callback (λ (c e)
                                            (change-to-directory (cdr (list-ref (send c get-path-elements)
                                                                                (get-field path-index e))))
                                            (send *dir-control set-path (path-alist main-directory)))]))
        (send *dir-control set-path (path-alist main-directory))
        (set! *popup-menu (new files-popup-menu%
                               [switch change-to-directory]
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
                                                                (message-box "Error" "Nothing to delete.")))]
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
                                       (define cmd (preferences:get 'files-viewer:cmd))
                                       (define p (if item (send item user-data) main-directory))
                                       (define path (if (file-exists? p) (simplify-path
                                                                          (build-path p 'up))
                                                        p))
                                       (if cmd (process/safe (string-replace cmd "~a" (~a path)))
                                           (message-box "Error" "Command to open terminal is undefined.")))]
                               [terminal-config-callback
                                (thunk (define cmd-config
                                         (new cmd-dialog% [parent this]))
                                       (send cmd-config show #t))]
                               [change-to-this-directory-callback
                                (thunk (let/ec exit
                                         (define item (send *files get-selected))
                                         (unless item (exit (message-box "Error" "Nothing is selected.")))
                                         (unless (directory-exists?
                                                  (send item user-data)) (exit (message-box "Error" "Not a directory.")))
                                         (change-to-directory (send item user-data))))]
                               [auto-refresh-status auto-refresh?]
                               [auto-refresh-callback
                                (λ (v)
                                  (set! auto-refresh? v)
                                  (preferences:set 'files-viewer:auto-refresh v)
                                  (if v
                                      (update-files!)
                                      (send fschange change-dirs '())))]
                               [change-to-the-directory-of-current-file-callback
                                (thunk 
                                 (define d (send (send this get-current-tab) get-directory))
                                 (change-to-directory d))]
                               [change-to-the-common-directory-callback
                                (thunk
                                 (define d
                                   (paths-common-prefix
                                    (for/list ([t (in-list (send this get-tabs))])
                                      (send t get-directory))))
                                 (when d (change-to-directory d)))]
                               [git-pull-callback
                                (thunk
                                 (define item (send *files get-selected))
                                 (define p (if item (send item user-data) main-directory))
                                 (define t (new terminal%))
                                 (thread (thunk (send t run-commands
                                                      (list "git pull origin master")
                                                      p ))))]
                               [git-push-callback
                                (thunk
                                 (define item (send *files get-selected))
                                 (define p (if item (send item user-data) main-directory))
                                 (new git-commit%
                                      [parent this]
                                      [content-callback (λ (c)
                                                          (define t (new terminal%))
                                                          (thread (thunk (send t run-commands
                                                                               (list "git add --all"
                                                                                     (format "git commit -m ~s" c)
                                                                                     "git push origin master")
                                                                               p
                                                                               ))))]))]
                               ))
        
          
        (set! *files (new directory-list% 
                          [parent real-area]
                          [select-callback (lambda (i)
                                             (define is-binary-file-open
                                               (preferences:get 'files-viewer:binary-file-open))
                                             (when (and (file-exists? i)
                                                        (or is-binary-file-open
                                                            (not (binary-file? i))))
                                               (cond
                                                 [(find-matching-tab i) => change-to-tab]
                                                 [(safe-to-change-file? (send (get-current-tab)  get-defs)) (change-to-file i)]
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

      (define/private (safe-to-change-file? ed)
        (define behavior (preferences:get 'files-viewer:behavior-open))
        (match behavior
          [0 (not (send ed is-modified?))]
          [1 (not (or (send ed is-modified?)
                      (send ed can-do-edit-operation? 'undo #t)
                      (send ed can-do-edit-operation? 'redo #t)
                      ))]
          [2 (not (or (send ed is-modified?)
                      (send ed can-do-edit-operation? 'undo #t)
                      (send ed can-do-edit-operation? 'redo #t)
                      (send ed get-filename)
                      ))]))

      (define/augment (on-close)
        (send fschange shutdown))
      (super-new)
      ))
  (drracket:get/extend:extend-unit-frame drracket-frame-mixin)
  
  )
