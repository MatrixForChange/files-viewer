#lang racket
(require syntax/parse/define (for-syntax racket/syntax) racket/gui)
(provide files-popup-menu%)
(define files-popup-menu%
  (class popup-menu%
    (super-new)
    (define-simple-macro (define-menu-item name:id label-name:str)
      #:with callback-name (format-id #'name "~a-callback" #'name)
      (begin (init-field [callback-name void])
             (define name (new menu-item% [label label-name]
                               [parent this]
                               [callback (λ (c e) (callback-name))]))))
    
    ;;------------------workspaces menu----------------------------------------
    
    (init switch)
    (define workspaces (new menu% [label "Workspaces"][parent this]))
    
    (define (get-workspaces+refresh)
      (define prefs (get-preference 'files-viewer:workspaces))
      (for ([i (send workspaces get-items)])
        (send i delete))
      (new menu-item%
           [label "Workspace Manager"]
           [parent workspaces]
           [callback (λ (c e) (send (new workspace-manager%) show #t))])
      (new separator-menu-item% [parent workspaces])
      (when prefs
        (for ([i prefs])
          (new menu-item% [label (first i)]
               [parent workspaces][callback (λ (c e)
                                              (switch (second i)))])))
      )

    
    (define/override (on-demand)
      (get-workspaces+refresh)
      (super on-demand))
    
    ;;------------------workspaces menu----------------------------------------


    
    (define-menu-item change-the-directory "Change the Directory")
    (define-menu-item change-to-this-directory "Change to this Directory")
    (define-menu-item change-to-the-directory-of-current-file
      "Change to the Directory of Current file")
    (define-menu-item refresh "Refresh")
    (new separator-menu-item% [parent this])
    (define-menu-item new-file "New")
    (define-menu-item delete-file "Delete")
    (define-menu-item rename-file "Rename")
    (define-menu-item file-filter "File Filter")
    (new separator-menu-item% [parent this])
    (define-menu-item open-terminal-here "Open Terminal Here")
    (define-menu-item terminal-config "Config for Terminal Launcher")
    
    (new separator-menu-item% [parent this])
    
    (init-field [auto-refresh-status #f]
                [auto-refresh-callback void])
    (define auto-refresh
      (new menu-item% [label (refresh-label auto-refresh-status)]
           [parent this]
           [callback (λ (c e)
                       (set! auto-refresh-status (not auto-refresh-status))
                       (send auto-refresh set-label (refresh-label auto-refresh-status))
                       (auto-refresh-callback auto-refresh-status))]))
    ))


(define (refresh-label v)
  (if v "Disable Auto Refresh" "Enable Auto Refresh"))

(define workspace-manager%
  (class frame%
    (super-new [label "Workspace Manager"][width 540][height 410])
    (define (refresh)
      (define prefs (get-preference 'files-viewer:workspaces))
      (when prefs
        (send ws set (map first prefs)
              (map second prefs))))
    (define ws (new list-box%
                    [parent this][choices '()]
                    [style '(column-headers single)]
                    [label ""][columns '("Name" "Path")]))
    (send ws set-column-width 0 200 150 10000)
    (send ws set-column-width 1 320 150 10000)
    (define bp (new horizontal-panel% [parent this][alignment '(right bottom)]
                    [stretchable-height #f]))
    (refresh)
   
    (define delete-button (new button% [label "Delete the Workspace"]
                               [parent bp]
                               [callback (λ (c e) (define s (send ws get-selections))
                                           (unless (null? s)
                                             (put-preferences '(files-viewer:workspaces)
                                                              (list
                                                               (for/list ([(x i)
                                                                           (in-indexed
                                                                            (get-preference 'files-viewer:workspaces))]
                                                                          #:unless (= (first s) i))
                                                                 x)))
                                             (refresh)
                                             ))]))
    (define new-button (new button% [label "Add New Workspace"]
                            [parent bp]
                            [callback (λ (c e)
                                        (send (new new-workspace%) show #t)
                                        (refresh))]))
    ))

(define new-workspace%
  (class dialog%
    (super-new [label "Add New Workspace"][width 475][height 180])
    
    (define p1 (new horizontal-panel% [parent this]))
    (new message% [parent p1][label "Name :"][stretchable-width #f]
         [min-width 45])
    (define name-text (new text-field% [parent p1][label ""]
                           ))

    (define p2 (new horizontal-panel% [parent this]))
    (new message% [parent p2][label "Path :"][stretchable-width #f]
         [min-width 45])
    (define path-text (new text-field% [parent p2][label ""]
                           ))
    
    (define bp (new horizontal-panel% [parent this][alignment '(right bottom)]
                    [stretchable-height #f]))

    (define dir-button (new button% [parent bp][label "Select the Directory"]
                            [callback (λ (c e)
                                        (define res (get-directory))
                                        (when res
                                          (send path-text set-value (path->string res))))]))
    (define cancel-button (new button% [parent bp][label "Cancel"][callback (λ (c e) (send this show #f))]))
    (define ok-button (new button% [parent bp][label "OK"]
                           [callback (λ (c e) (define prefs (get-preference 'files-viewer:workspaces))
                                       (set! prefs (if prefs prefs '()))
                                       (put-preferences '(files-viewer:workspaces)
                                                        (list (append prefs (list (list (send name-text get-value)
                                                                                        (send path-text get-value))))))
                                       (send this show #f))]))
    (define/override (on-subwindow-char recv ev)
      (when (equal? (send ev get-key-code) #\return)
        (send ok-button command (make-object control-event% 'button (current-milliseconds))))
      (super on-subwindow-char recv ev))
    ))
    
                                              
    

(module+ test1
  (new files-popup-menu% [change-the-directory-callback void]
       ))