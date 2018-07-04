#lang racket
(require mrlib/hierlist racket/gui framework)
(provide directory-list% my-horizontal-dragable%)


(define my-horizontal-dragable%
  (class panel:horizontal-dragable%
      (define/augment (get-default-percentages i)
                         (cond
                           [(= i 2) 
                            (list #e0.15 #e0.85)]
                           [else (build-list i (λ (x) (/ i)))]))
    (super-new)))
(define text-mixin
  (mixin (hierarchical-list-item<%>)
    ((interface () set-text get-text))
    (inherit get-editor)
    (super-new)
    ; set-text: this sets the label of the item
    (define/public (set-text str)
      (define t (get-editor)) ; a text% object
      (send t erase)
      (send t insert str))
    (define/public (get-text)
      (define t (get-editor))
      (send t get-text))

    ))


(define directory-list%
  (class hierarchical-list%
    (init-field [select-callback (λ (_) (void))])
    (super-new)
    (define the-dir #f)
    (inherit delete-item get-items)
    (define/public (update-files!)
      (for-each (λ (x) (delete-item x)) (get-items))
      (when (and the-dir (directory-exists? the-dir))
        (update-directory! this the-dir)))

    (define/public (set-dir! dir)
      (set! the-dir dir))

    (define (update-directory! parent dir)
      (for ([i (directory-list dir)])
        (define is-directory (directory-exists? (build-path dir i)))
        (define item (if is-directory
                         (send parent new-list text-mixin)
                         (send parent new-item text-mixin)))
        (send item user-data (build-path dir i))
        (send item set-text (path->string i))
        (when is-directory
          (update-directory! item (build-path dir i)))
        ))

    (define/override (on-select i)
      (when i (select-callback (send i user-data))))

    
    ))

(module+ test
  (define f (new frame% [label "hierlist demo"][width 400][height 500]))
  (define dl (new directory-list% [parent f]))
  (send dl set-dir! "d:/minecraft")
  (send dl update-files!)
  (send f show #t))
                               
      