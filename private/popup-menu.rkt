#lang racket
(require syntax/parse/define (for-syntax racket/syntax) racket/gui
         framework)
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
    (define workspaces (new menu% [label "Workspaces"] [parent this]))
    
    (define (get-workspaces+refresh)
      (define prefs (preferences:get 'files-viewer:workspaces))
      (for ([i (send workspaces get-items)])
        (send i delete))
      (new menu-item%
           [label "Workspace manager"]
           [parent workspaces]
           [callback (λ (c e) (send (new workspace-manager%) show #t))])
      (when (not (empty? prefs))
        (new separator-menu-item% [parent workspaces])
        (for ([i prefs])
          (new menu-item% [label (first i)]
               [parent workspaces][callback (λ (c e)
                                              (switch (second i)))])))
      )

    
    (define/override (on-demand)
      (get-workspaces+refresh)
      (super on-demand))
    
    ;;------------------workspaces menu----------------------------------------


    
    (define-menu-item change-the-directory "Change the directory")
    (define-menu-item change-to-this-directory "Change to this directory")
    (define-menu-item change-to-the-directory-of-current-file
      "Change to the directory of current File")
    (define-menu-item change-to-the-common-directory
      "Change to the common directory")
    (define-menu-item refresh "Refresh")
    (new separator-menu-item% [parent this])
    (define-menu-item new-file "New")
    (define-menu-item delete-file "Delete")
    (define-menu-item rename-file "Rename")
    (define-menu-item file-filter "File filter")
    (new separator-menu-item% [parent this])
    (define-menu-item open-terminal-here "Open terminal here")
    (define-menu-item terminal-config "Configure the terminal launcher")
    (new separator-menu-item% [parent this])
    (define-menu-item git-pull "git pull")
    (define-menu-item git-push "git push")
    
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
    (new menu-item% [label "Extra settings"]
         [parent this]
         [callback (λ (c e) (send (new extra-settings%) show #t))])
    ))
 

(define (refresh-label v)
  (if v "Disable auto refresh" "Enable auto refresh"))


;; TODO: add double-click & right-click context menu item for editing workspace.
(define workspace-manager%
  (class frame%
    (super-new [label "Workspace manager"] [width 540] [height 410])
    (define (refresh)
      (define prefs (preferences:get 'files-viewer:workspaces))
      (when prefs
        (send ws set (map first prefs)
              (map second prefs))))
    (define ws (new list-box%
                    [parent this][choices '()]
                    [style '(column-headers single)]
                    [label ""][columns '("Name" "Path")]
                    [callback (λ (list-box event)
                                (when (equal? (send event get-event-type) 'list-box-dclick)
                                  ;; TODO: complete this.
                                  (void)))]))
    (send ws set-column-width 0 200 150 10000)
    (send ws set-column-width 1 320 150 10000)
    (define bp (new horizontal-panel% [parent this][alignment '(right bottom)]
                    [stretchable-height #f]))
    (refresh)
   
    (define delete-button (new button% [label "Delete the workspace"]
                               [parent bp]
                               [callback (λ (c e) (define s (send ws get-selections))
                                           (unless (null? s)
                                             (preferences:set 'files-viewer:workspaces
                                                               (for/list ([(x i)
                                                                           (in-indexed
                                                                            (preferences:get 'files-viewer:workspaces))]
                                                                          #:unless (= (first s) i))
                                                                 x))
                                             (refresh)
                                             ))]))
    (define new-button (new button% [label "Add new workspace"]
                            [parent bp]
                            [callback (λ (c e)
                                        (send (new new-workspace%) show #t)
                                        (refresh))]))))

(define new-workspace%
  (class dialog%
    (super-new [label "Add a new workspace"] [width 475] [height 180])
    
    (define p1 (new horizontal-panel% [parent this]))
    (new message%
         [parent p1]
         [label "Name:"]
         [stretchable-width #f]
         [min-width 45])
    (define name-text (new text-field% [parent p1] [label ""]))

    (define p2 (new horizontal-panel% [parent this]))
    (new message%
         [parent p2]
         [label "Path:"]
         [stretchable-width #f]
         [min-width 45])
    (define path-text (new text-field% [parent p2] [label ""]))
    
    (define bp (new horizontal-panel%
                    [parent this]
                    [alignment '(right bottom)]
                    [stretchable-height #f]))

    (define dir-button (new button%
                            [parent bp]
                            [label "Select the directory"]
                            [callback (λ (c e)
                                        (define res (get-directory))
                                        (when res
                                          (send path-text set-value (path->string res))))]))
    (define cancel-button (new button% [parent bp] [label "Cancel"] [callback (λ (c e) (send this show #f))]))
    (define ok-button (new button%
                           [parent bp]
                           [label "OK"]
                           [callback (λ (c e) (define prefs (preferences:get 'files-viewer:workspaces))
                                        (preferences:set 'files-viewer:workspaces
                                                         (append prefs (list (list (send name-text get-value)
                                                                                   (send path-text get-value)))))
                                        (send this show #f))]))
    (define/override (on-subwindow-char recv ev)
      (when (equal? (send ev get-key-code) #\return)
        (send ok-button command (make-object control-event% 'button (current-milliseconds))))
      (super on-subwindow-char recv ev))
    (send name-text focus)))

;; TODO: complete this.
(define edit-workspace%
  (class frame%
    (super-new [label "Edit workspace"]
               [width 475] [height 180])
    (define btn-save (new button%
                          [label "Save"]
                          [callback (λ (btn e)
                                      (void)
                                      )]))
    (define btn-cancel (new button%
                            [label "Cancel"]
                            [callback (λ (btn e)
                                        (void)
                                        )]))
    ))

(define extra-settings%
  (class frame%
    (super-new [width 400][height 300][label "Extra settings"])
    (define tp (new tab-panel% [parent this][choices '("Legacy features"
                                                       "Experimental features")]
                    [callback (λ (c e) (update-panels))]))
    (define change-current-tab-to-a-new-file-when
      (new radio-box%
           [label "Change current tab to a new file when..."]
           [choices '("Current tab has no changes to save (old)"
                      "Current tab can't do any redo and undo operations (old)"
                      "Current tab has a file name (default)")]
           [parent tp]
           [selection (preferences:get 'files-viewer:behavior-open)]
           [callback (λ (c e)
                       (preferences:set 'files-viewer:behavior-open
                                        (send change-current-tab-to-a-new-file-when
                                              get-selection)))]))

    (define is-binary-file-open
      (new check-box%
           [label "Open a known binary file in a new tab."]
           [parent tp]
           [value (preferences:get 'files-viewer:binary-file-open)]
           [callback (λ (c e)
                       (preferences:set 'files-viewer:binary-file-open
                                        (send is-binary-file-open get-value)))]))

    (define (update-panels)
      (send tp change-children (λ (l)
                                 (match (send tp get-selection)
                                   [0 (list change-current-tab-to-a-new-file-when)]
                                   [1 (list is-binary-file-open)]))))
    (update-panels)))
                                              
    

(module+ test1
  (new files-popup-menu% [change-the-directory-callback void]))
