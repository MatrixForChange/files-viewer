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
    (define-menu-item change-the-directory "Change the Directory")
    (define-menu-item change-to-this-directory "Change to this Directory")
    (define-menu-item change-to-the-directory-of-current-file
      "Change to the directory of current file")
    (define-menu-item parent-directory "Up")
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

(module+ test1
  (new files-popup-menu% [change-the-directory-callback void]
       ))