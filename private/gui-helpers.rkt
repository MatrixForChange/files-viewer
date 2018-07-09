#lang racket
(require mrlib/hierlist racket/gui framework racket/runtime-path file/glob)
(provide directory-list% my-horizontal-dragable%)

(define my-horizontal-dragable%
  (class panel:horizontal-dragable%
    (inherit get-percentages)
    (define/augment (get-default-percentages i)
      (cond
        [(= i 2)
         (define default-percentages (get-preference 'files-viewer:percentages))
         (if default-percentages default-percentages (list #e0.15 #e0.85))]
        [else (build-list i (λ (x) (/ i)))]))
    (define/augment (after-percentage-change)
      (define current (get-percentages))
      (when (= (length current) 2)
        (put-preferences '(files-viewer:percentages) (list current)))
      (inner (void) after-percentage-change))
    (super-new)))

;;; generated using https://gist.github.com/yjqww6/a102dffb7e2ad00685a60da5e7469f88
(define-runtime-path racket-icon "doc.png")
(define-runtime-path normal-icon "normal.png")
(define file-icon-snip
  (let ([r (make-object bitmap% 1 1)]
        [n (make-object bitmap% 1 1)]
        [g '("*.rkt" "*.scrbl" "*.rktl" "*.rktd" "*.ss" "*.scm")])
    (send r load-file racket-icon)
    (send n load-file normal-icon)
    (λ (str)
      (define (is-racket? name)
        (glob-match? g name))
      (define s (make-object image-snip%))
      (send s set-bitmap (if (is-racket? str) r n))
      s)))

(define simple-mixin
  (mixin (hierarchical-list-item<%>)
    ((interface () set-text get-text))
    (inherit get-editor)
    (super-new)
    

    (define/public (set-text str)
      (define t (get-editor))
      (send t erase)
      (send t insert (file-icon-snip str))
      (send t insert " ")
      (send t insert str))
    (define/public (get-text)
      (define t (get-editor))
      (send t get-text))
    ))

(define compound-mixin
  (mixin (hierarchical-list-compound-item<%>)
    ((interface () set-text get-text))
    (inherit get-editor)
    (super-new)
    (define task void)
    (define ran #f)
    (define/public (set-text str)
      (define t (get-editor))
      (send t erase)
      (send t insert str))
    (define/public (get-text)
      (define t (get-editor))
      (send t get-text))
    (define/public (set-task thunk)
      (set! task thunk))

    (define/public (run-task)
      (unless ran
        (task)
        (set! ran #t)))
    ))


(define directory-list%
  (class hierarchical-list%
    (init-field [select-callback void]
                [my-popup-menu #f])
    (super-new)
    (inherit delete-item get-items popup-menu allow-deselect)
    (define the-dir #f)
    (define opened (mutable-set))
    (allow-deselect #t)
    (define/public (update-files!)
      (define filter-types (get-preference 'files-viewer:filter-types))
      (for-each (λ (x) (delete-item x)) (get-items))
      (when (and the-dir (directory-exists? the-dir))
        (update-directory! this the-dir (if filter-types filter-types '()))))

    (define/public (set-dir! dir)
      (set! the-dir dir))

    (define (update-directory! parent dir filter-types)
      (let/ec exit
        (define files
          (with-handlers ([exn:fail?
                           (λ (e)
                             (exit
                              (message-box "error" "can't open the directory")))])
            (directory-list
             dir)))
        (for ([i files])
          (unless (ormap (λ (x) (path-has-extension? i x)) filter-types)
            (define is-directory (directory-exists? (build-path dir i)))
            (define item (if is-directory
                             (send parent new-list compound-mixin)
                             (send parent new-item simple-mixin)))
            (send item user-data (build-path dir i))
            (send item set-text (path->string i))
            (when is-directory
              (send item set-task (thunk (update-directory! item (build-path dir i) filter-types)))
              (when (set-member? opened (send item user-data))
                (send item open))
              )))))

    (define/override (on-double-select i)
      (when i
        (select-callback (send i user-data))
        )
      (super on-double-select i)
      )

    
    
    (define/override (on-event ev)
      (super on-event ev)
      (when (send ev button-down? 'right)
        (popup-menu my-popup-menu (send ev get-x)
                    (send ev get-y)))
      )
    (define/override (on-item-opened item)
      (send item run-task)
      (set-add! opened (send item user-data))
      (super on-item-opened item))
    (define/override (on-item-closed item)
      (set-remove! opened (send item user-data))
      (super on-item-closed item)) 
    ))




(module+ test1
  (define f (new frame% [label "hierlist demo"][width 400][height 500]))
  (define dl (new directory-list% [parent f]))
  (send dl set-dir! "d:/minecraft")
  (send dl update-files!)
  (send f show #t))
                               
