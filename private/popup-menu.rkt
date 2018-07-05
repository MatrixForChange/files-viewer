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
    (define-menu-item refresh "Refresh")
    (define-menu-item new-file "New File")
    (define-menu-item delete-file "Delete File")
    (define-menu-item file-filter "File Filter")
    ))

(module+ test1
  (new files-popup-menu% [change-the-directory-callback void]
       ))