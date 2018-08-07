#lang racket
(require racket/gui)
(provide rename-dialog% cmd-dialog%)
(define rename-dialog%
  (class dialog%
    (super-new [label "Rename File or Directory"][width 400][height 100])
    (init-field path)
    (define f (new text-field% [label "New Name:"] [parent this]))
    (define ok (new button% [label "OK"][parent this]
                    [callback (λ (c e)
                                (with-handlers
                                    ([exn:fail? (λ (e)
                                                  (message-box "error" "file name invalid."))])
                                  (rename-file-or-directory path
                                                            (simplify-path (build-path path 'up (send f get-value)))))
                                (send this show #f))])
      )
    (define/override (on-subwindow-char recv ev)
      (when (equal? (send ev get-key-code) #\return)
        (send ok command (make-object control-event% 'button (current-milliseconds))))
      (super on-subwindow-char recv ev))
    ))
(define cmd-dialog%
  (class dialog%
    (super-new [label "Set the Command for Terminal Launcher"][width 500][height 160])
    (define m (new message% [label "~a stands for the directory,such as start /d ~a cmd"]
                   [parent this]))
    (define f (new text-field%
                   [label "Command:"]
                   [init-value (let ([old (get-preference 'files-viewer:cmd)])
                                 (if old old ""))]
                   [parent this]))
    (define ok (new button% [label "OK"][parent this]
                    [callback (λ (c e)
                                (put-preferences '(files-viewer:cmd) (list (send f get-value)))
                                (send this show #f))])
      )
    (define/override (on-subwindow-char recv ev)
      (when (equal? (send ev get-key-code) #\return)
        (send ok command (make-object control-event% 'button (current-milliseconds))))
      (super on-subwindow-char recv ev))
    ))
