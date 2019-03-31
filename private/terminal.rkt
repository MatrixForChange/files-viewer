#lang racket/gui
(require framework)
(provide terminal% git-commit%)
;;Provide terminal window

(define terminal%
  (class frame%
    (super-new [width 600]
               [height 480]
               [label "Terminal"]
               )
    (define edt (new terminal-editor%))
    (define ec (new editor-canvas% [parent this][editor edt]))
    
    (define/public (run-commands lst dir)
      (for ([cmd (in-list lst)])
        (send edt output (format "Running:~a\n" cmd))
        (match-define (list stdout stdin pid stderr control)
          (parameterize ([current-directory dir])
            (process cmd)))
        (send edt output (port->string stdout))
        (send edt output (port->string stderr))
        (close-output-port stdin)
        )
      (send edt output "Work Done!\n")
      )
    (inherit show)
    (show #t)))

(define terminal-editor%
  (class color:text%
    (super-new [auto-wrap #t])
    (inherit insert last-position begin-edit-sequence end-edit-sequence)
    (define lock #f)
    (define/augment (can-insert? s l)
      lock)
    (define/augment (can-delete? s l)
      lock)
    (define/public (output t)
      (set! lock #t)
      (begin-edit-sequence)
      (insert t (last-position))
      (end-edit-sequence)
      (set! lock #f))
    ))


(define git-commit%
  (class dialog%
    [init-field content-callback]
    (super-new [width 420][height 130][label "Commit Message:"])
    (define/override (on-subwindow-char receiver event)
      (when (eqv? (send event get-key-code) #\return)
        (send this show #f)
        (content-callback (send tf get-value)))
      (super on-subwindow-char receiver event))
    (define tf (new text-field% [label ""][parent this]))
    (define panel (new horizontal-panel% [parent this][alignment '(right bottom)]))
    (define ok (new button% [label "OK"][parent panel][callback (λ (c e)
                                                                  (send this show #f)
                                                                  (content-callback (send tf get-value)))]))
    (send tf focus)
    (send this show #t)))

