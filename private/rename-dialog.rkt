#lang racket
(require racket/gui framework)
(provide rename-dialog% cmd-dialog%)
(define rename-dialog%
  (class dialog%
    (super-new [label "Rename file or directory"][width 475][height 100])
    (init-field path)
    (define f (new text-field% [label "New name:"] [parent this]))
    (define p (new horizontal-panel% [parent this] [alignment '(right bottom)]))
    (define cancel (new button%
                        [label "Cancel"]
                        [parent p]
                        [callback (λ (c e) (send this show #f))]))
    (define ok
      (new button%
           [label "OK"]
           [parent p]
           [callback
            (λ (c e)
              (with-handlers
                ([exn:fail? (λ (e) (message-box "Error" "Invalid file name."))])
                (rename-file-or-directory
                 path
                 (simplify-path (build-path path 'up (send f get-value)))))
              (send this show #f))])
      )
    (send f focus)
    (define/override (on-subwindow-char recv ev)
      (when (equal? (send ev get-key-code) #\return)
        (send ok command (make-object control-event% 'button (current-milliseconds))))
      (super on-subwindow-char recv ev))
    ))
(define cmd-dialog%
  (class dialog%
    (super-new
     [label "Set the command for terminal launcher"]
     [width 525]
     [height 160])
    (define f (new text-field%
                   [label "Command: "]
                   [init-value (or (preferences:get 'files-viewer:cmd) "")]
                   [parent this]))
    (new message%
         [label (string-append "Command example: start /d ~a cmd\n"
                               "where ~a is a placeholder for the directory")]
         [parent this])

    (define p (new horizontal-panel% [parent this] [alignment '(right bottom)]))
    (define cancel (new button%
                        [label "Cancel"]
                        [parent p]
                        [callback (λ (c e) (send this show #f))]))
    (define ok (new button%
                    [label "OK"]
                    [parent p]
                    [callback (λ (c e)
                                (preferences:set 'files-viewer:cmd (send f get-value))
                                (send this show #f))]))
    (send f focus)
    (define/override (on-subwindow-char recv ev)
      (when (equal? (send ev get-key-code) #\return)
        (send ok command (make-object control-event% 'button (current-milliseconds))))
      (super on-subwindow-char recv ev))))
