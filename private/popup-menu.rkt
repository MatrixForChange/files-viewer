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
                                  ;; NOTE: this will only take the first selected workspace if
                                  ;; multiple workspace is selected, as occasions where you need
                                  ;; to edit multiple workspace at once is kinda rare.
                                  ;; `get-string-selection` only gets the first column.
                                  ;; `get-data` with `get-selection` somehow does not work (will
                                  ;; only returns #f)
                                  (let ([item (send list-box get-string-selection)])
                                    (when item
                                      (send (new edit-workspace% [workspace-name item]) show #t)
                                      (refresh)))))]))
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
    (define edit-button (new button% [parent bp]
                             [label "Edit workspace"]
                             [callback (λ (c e)
                                         (let ([item (send ws get-string-selection)])
                                           (when item
                                             (send (new edit-workspace% [workspace-name item]) show #t)
                                             (refresh))))]))
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
    (define name-text (new text-field% [parent p1] [label ""] [callback (λ (c e) (enable-ok-button))]))

    ; Check to see if the name-text field contains a non-empty string and
    ; the path-text field contains a valid directory path.
    ; Enable or disable the OK button as appropriate.
    (define/private (enable-ok-button)
      (let ([path-string (send path-text get-value)])
        (send ok-button enable
              (and (non-empty-string? (string-trim (send name-text get-value)))
                   (non-empty-string? path-string)
                   (directory-exists? (string->path path-string))))))
    
    (define p2 (new vertical-panel% [parent this]))
    (define p2txt (new horizontal-panel% [parent p2] [stretchable-height #f]))
    (new message%
         [parent p2txt]
         [label "Path:"]
         [stretchable-width #f]
         [min-width 45])
    (define path-text (new text-field% [parent p2txt] [label ""] [callback (λ (c e) (enable-ok-button))]))
    
    (define p2dir (new horizontal-panel% [parent p2] [alignment '(right top)] [stretchable-height #f]))
    (define current-button (new button%
                                [parent p2dir]
                                [label "Use current directory"]
                                [callback (λ (c e)
                                            (send path-text set-value
                                                  (preferences:get 'files-viewer:directory))
                                            (enable-ok-button))]))
    (define dir-button (new button%
                            [parent p2dir]
                            [label "Select the directory"]
                            [callback (λ (c e)
                                        (define res (get-directory))
                                        (when res
                                          (send path-text set-value (path->string res))
                                          (enable-ok-button)))]))
    (define bp (new horizontal-panel%
                    [parent this]
                    [alignment '(right bottom)]
                    [stretchable-height #f]))
    (define ok-button (new button%
                           [parent bp]
                           [label "OK"]
                           [enabled #f]
                           [callback (λ (c e) (define prefs (preferences:get 'files-viewer:workspaces))
                                       (preferences:set 'files-viewer:workspaces
                                                        (append prefs (list (list (send name-text get-value)
                                                                                  (send path-text get-value)))))
                                       (send this show #f))]))
    (define cancel-button (new button% [parent bp] [label "Cancel"] [callback (λ (c e) (send this show #f))]))
    (define/override (on-subwindow-char recv ev)
      (when (equal? (send ev get-key-code) #\return)
        (send ok-button command (make-object control-event% 'button (current-milliseconds))))
      (super on-subwindow-char recv ev))
    (send name-text focus)))


;; NOTE: "add new workspace" & "edit existing workspace" are essentially the same thing
;; in the current setting but I'm not sure if reusing the same code will add trouble
;; when the two functionalities become different in the future.
(define edit-workspace%
  (class dialog%
    (super-new [label "Edit workspace"]
               [width 475] [height 180])
    ;; NOTE: this is required to get related info.
    (init-field workspace-name)

    (define hp1 (new horizontal-panel% [parent this]))
    (new message% [parent hp1]
         [label "Name:"]
         [stretchable-width #f]
         [min-width 45])
    (define tf-name (new text-field% [parent hp1]
                         [label ""]))
    (send tf-name set-value workspace-name)

    (define hp2 (new horizontal-panel% [parent this]))
    (new message% [parent hp2]
         [label "Path:"]
         [stretchable-width #f]
         [min-width 45])
    (define tf-path (new text-field% [parent hp2]
                         [label ""]))
    (let* ([workspace-data (preferences:get 'files-viewer:workspaces)]
           [lookup-res (assoc workspace-name workspace-data)])
      (when lookup-res
        (send tf-path set-value (cadr lookup-res))))

    (define hp3 (new horizontal-panel% [parent this]
                     [alignment '(right bottom)]
                     [stretchable-height #f]))
    (define btn-dir (new button% [parent hp3]
                         [label "Select directory..."]
                         [callback (λ (c e)
                                     (let ([res (get-directory)])
                                       (when res
                                         (send tf-path set-value (path->string res)))))]))
    (define btn-save (new button% [parent hp3]
                          [label "Save"]
                          [callback
                           (λ (btn e)
                             (let* ([workspace-data (preferences:get 'files-viewer:workspaces)]
                                    [edited-data
                                     ;; NOTE: when the name is changed the record with the original name
                                     ;; shall be deleted.
                                     (dict-set (dict-remove workspace-data workspace-name)
                                               (send tf-name get-value)
                                               (list (send tf-path get-value)))])
                               (preferences:set 'files-viewer:workspaces
                                                edited-data))
                             (send this show #f))]))
    (define btn-cancel (new button% [parent hp3]
                            [label "Cancel"]
                            [callback (λ (btn e)
                                        (send this show #f))]))

    (define/override (on-subwindow-char recv ev)
      (when (equal? (send ev get-key-code) #\return)
        (send btn-save command (make-object control-event% 'button (current-milliseconds))))
      (super on-subwindow-char recv ev))))

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

(module+ test2
  (send (new new-workspace%) show #t))
