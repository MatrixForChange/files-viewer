#lang racket/gui

(provide instruction-dialog%)
(define instruction-dialog%
  (class dialog%
    (init [content-callback (λ (c) (void))])
    (super-new [label "Create a info.rkt"]
               [width 600]
               [height 480])
    (define pkg-name (new text-field% [parent this][label "package name:"]))
    (define pkg-version (new text-field% [parent this][label "package version:"]))
    (define pkg-collection (new radio-box% [label "package collections:  "]
                                [parent this]
                                [choices '("use package name"
                                           "multi collections"
                                           "single collection")]
                                [stretchable-width #t]
                                [callback (λ (c e)
                                            (send pkg-collection-name enable (= (send c get-selection) 2))
                                            )]))
    (define pkg-collection-name (new text-field% [parent this]
                                     [label "package collection name: "]
                                     [enabled #f]))
    (define dep-msg (new message% [parent this][label "dependencies: "][stretchable-width #t]))

    (define (generate-control x)
      (new check-box% [parent dependencies]
           [stretchable-width #t]
           [label x]))
    (define (update-panels)
      (send dependencies change-children
            (λ (l) (match (send dependencies get-selection)
                     [0 base-libs]
                     [1 gui-libs]))))
    (define (get-libs lst)
      (filter values (map (lambda (x)
                            (if (send x get-value)
                                (send x get-label)
                                #f)) lst)))
    (define dependencies (new tab-panel% [parent this][choices (list "Base Libs" "Gui Libs")]
                              [callback (λ (c e) (update-panels))]))
    (define base-libs (map generate-control '("base" "rackunit-lib" "scheme-lib"
                                                     "compatibility-lib"
                                                     "threading"
                                                     )))
    (define gui-libs (map generate-control
                          '("gui-lib" "scribble-lib" "pict-lib" "plot-gui-lib")))

    
    (update-panels)
    (define ok-button (new button% [parent this][label "OK"][stretchable-width #t]
                           [callback (λ (c e)
                                       (define sexps `((define name ,(send pkg-name get-value))
                                                       (define version ,(send pkg-version get-value))
                                                       (define collection ,(match (send pkg-collection get-selection)
                                                                             [0 (send pkg-name get-value)]
                                                                             [1 'multi]
                                                                             [2 (send pkg-collection-name get-value)]))
                                                       (define deps ,(append (get-libs base-libs)
                                                                             (get-libs gui-libs)))))
                                       (content-callback (string-append "#lang info\n"
                                               (string-join
                                                (map (lambda (x)
                                                       (call-with-output-string (λ (p)
                                                                                  (write x p))))
                                                     sexps)
                                                "\n")))
                                       (send this show #f)
                                       )]))
    ))


