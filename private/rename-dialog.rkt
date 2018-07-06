#lang racket
(require racket/gui)
(provide rename-dialog%)
(define rename-dialog%
  (class dialog%
    (super-new [label "Rename File or Directory"][width 400][height 100])
    (init-field path)
    (define f (new text-field% [label "New Name:"] [parent this]))
    (define ok (new button% [label "OK"][parent this]
                    [callback (λ (c e)
                                (rename-file-or-directory path
                                                          (simplify-path (build-path path 'up (send f get-value))))
                                (send this show #f))])
                                )))
    