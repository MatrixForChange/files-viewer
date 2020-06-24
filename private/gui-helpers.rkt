#lang racket
(require "../hierlist/hierlist.rkt" racket/gui framework racket/runtime-path file/glob pict)
(provide directory-list% my-horizontal-dragable%)

(define my-horizontal-dragable%
  (class panel:horizontal-dragable%
    (inherit get-percentages)
    (define/augment (get-default-percentages i)
      (cond
        [(= i 2)
         (define default-percentages (preferences:get 'files-viewer:percentages))
         (if default-percentages default-percentages (list #e0.15 #e0.85))]
        [else (build-list i (λ (x) (/ i)))]))
    (define/augment (after-percentage-change)
      (define current (get-percentages))
      (when (= (length current) 2)
        (preferences:set 'files-viewer:percentages current))
      (inner (void) after-percentage-change))
    (super-new)))

;;; generated using https://gist.github.com/yjqww6/a102dffb7e2ad00685a60da5e7469f88
(define-runtime-path racket-icon-path "icons/doc.png")
(define-runtime-path normal-icon-path "icons/normal.png")

(define (fit path)
  (pict->bitmap
   (scale-to-fit (bitmap (make-object bitmap% path 'png/alpha #f #f 2)) 16 16)
   #:make-bitmap (λ (w h) (make-bitmap w h #t #:backing-scale 2))))
(define racket-icon (fit racket-icon-path))
(define normal-icon (fit normal-icon-path))
(define file-icon-snip
  (let ([g '("*.rkt" "*.scrbl" "*.rktl" "*.rktd" "*.ss" "*.scm")])
    (λ (str)
      (define (is-racket? name)
        (glob-match? g name))
      (define s (make-object image-snip%))
      (send s set-bitmap (if (is-racket? str) racket-icon normal-icon))
      s)))


(define simple-mixin
  (mixin (hierarchical-list-item<%>)
    ((interface () set-text get-text))
    (inherit get-editor)
    (super-new)
    (define red-style (make-object style-delta%))
    (send red-style set-delta-foreground "red")
    (define/public (set-text str where)
      (define t (get-editor))
      (send t erase)
      (send t insert (file-icon-snip str))
      (send t insert " ")
      (send t insert str)
      (send t change-style (make-object style-delta% 'change-alignment 'top) 0 (send t last-position))
      (send t change-style red-style (+ (car where) 2) (+ (cdr where) 2)))
    (define/public (get-text)
      (define t (get-editor))
      (send t get-text))
    (define/public (compound?)
      #f
      )
    ))

(define compound-mixin
  (mixin (hierarchical-list-compound-item<%>)
    ((interface () set-text get-text))
    (inherit get-editor delete-item get-items)
    (super-new)

    (field [hierlist #f])
    
    (define/public (set-text str)
      (define t (get-editor))
      (send t erase)
      (send t insert " ")
      (send t insert str)
      )
    (define/public (get-text)
      (define t (get-editor))
      (send t get-text))

    (define/public (on-opened)
      (define t (get-editor))
      (when hierlist
        (send t begin-edit-sequence)
        (send hierlist update-directory! this (send this user-data)
              (or (preferences:get 'files-viewer:filter-types) '()))
        (send t end-edit-sequence)
        (send hierlist refresh)))

    (define/public (on-closed)
      (define t (get-editor))
      (send t begin-edit-sequence)
      (for-each (λ (x) (delete-item x)) (get-items))
      (send t end-edit-sequence)
      (when hierlist
        (send hierlist refresh)))
    
    (define/public (compound?)
      #t
      )
    ))

(define ((item-<? path dir?) y)
  (define p2 (send y compound?))
  (cond [(xor dir? p2) dir?]
        [else (path<? path (send y user-data))]))

(define directory-list%
  (class hierarchical-list%
    (init-field [select-callback void]
                [my-popup-menu #f]
                [opened-change-callback void] ;change by manually click
                )
    (super-new)
    (inherit delete-item get-items popup-menu allow-deselect get-editor
             suspend-flush resume-flush refresh get-selected
             )
    (define the-dir #f)
    (define opened (mutable-set))
    (allow-deselect #t)
    (define/override (on-char ev)
      (define c (send ev get-key-code))
      (define old (send (get-editor) get-word-filter))
      (match c
        [#\backspace (send (get-editor) set-word-filter
                           (if (string=? old "")
                               ""
                               (substring old 0 (- (string-length old) 1))))
                     (update-files!)]
        [(or (? (conjoin char? char-graphic?)) #\space)
         (send (get-editor) set-word-filter (string-append old (string c)))
         (update-files!)]
        [else (void)])
      (super on-char ev))
    
    (define/public (update-files! [changed-paths #f])
      (define e (get-editor))
      (define ad (send e get-admin))
      (define filter-types (preferences:get 'files-viewer:filter-types))
      (suspend-flush)
      (send e begin-edit-sequence)
      (define-values (x y w h) (values (box 0) (box 0) (box 0) (box 0)))
      (send ad get-view x y w h)
      
      (cond
        [(or (not changed-paths) (member the-dir changed-paths))
         (for-each (λ (x) (delete-item x)) (get-items))
         (when (and the-dir (directory-exists? the-dir))
           (update-directory! this the-dir (or filter-types '())))]
        [else
         (define changed-items
           (let loop ([items (get-items)] [changed-items '()])
             (cond
               [(null? items) changed-items]
               [(not (send (car items) compound?))
                (loop (cdr items) changed-items)]
               [(member (send (car items) user-data) changed-paths)
                (define children (send (car items) get-items))
                (loop (cdr items) (loop children (cons (car items) changed-items)))]
               [else
                (define children (send (car items) get-items))
                (loop (cdr items) (loop children changed-items))])))
         (for ([item (in-list changed-items)])
           (update-directory! item (send item user-data) (or filter-types '()) #t))])
      
      (send e end-edit-sequence)
      (send ad scroll-to (unbox x) (unbox y) (unbox w) (unbox h) #f)
      (resume-flush)
      (refresh))

    (define/public (set-dir! dir)
      (set! the-dir dir))

    (define/public-final (update-directory! parent dir filter-types [delta? #f])
      (define files
        (with-handlers ([exn:fail? (λ (e) '())])
          (directory-list dir)))
      (define cute-syntax-enabled? (string-prefix? (send (get-editor) get-word-filter)
                                                   "`"))
      (define compiled-regexp
        (with-handlers ([exn:fail?
                         (λ (e)
                           #f)])
          (regexp (format "(?i:~a)" (send (get-editor) get-word-filter)))))

      (define-values (dirs regular-files)
        (partition (λ (p) (directory-exists? (build-path dir p)))
                   (sort files path<?)))

      (define ((add-item! is-directory) i)
        (when (and (or is-directory
                       (not (xor (preferences:get 'files-viewer:filter-types2)
                                 (ormap (λ (x) (path-has-extension? i x)) filter-types))))
                   (not (and (preferences:get 'files-viewer:filter-types3) (string-prefix? (path->string i) ".")))
                   (or is-directory
                       cute-syntax-enabled?
                       (not compiled-regexp)
                       (regexp-match compiled-regexp
                                     (path->string i)))
                   )
          (define item
            (cond
              [delta?
               (define items (send parent get-items));ordered
               (define before-it (findf (item-<? (build-path dir i) is-directory) items))
               (if before-it
                   (if is-directory
                       (send parent new-list-before compound-mixin before-it)
                       (send parent new-item-before simple-mixin before-it))
                   (if is-directory
                       (send parent new-list compound-mixin)
                       (send parent new-item simple-mixin)))]
              [is-directory
               (send parent new-list compound-mixin)]
              [else
               (send parent new-item simple-mixin)]))
          (send item user-data (build-path dir i))
          (cond [is-directory (send item set-text (path->string i))]
                [(and compiled-regexp
                      (not cute-syntax-enabled?))
                 (send item set-text (path->string i) (car
                                                       (regexp-match-positions
                                                        compiled-regexp
                                                        (path->string i))))]
                [else (send item set-text (path->string i) (cons 0 0))])
          (when is-directory
            (set-field! hierlist item this)
            (when (set-member? opened (send item user-data))
              (send item open))
            )))

      (cond
        [(not delta?)
         (for-each (add-item! #t) dirs)
         (for-each (add-item! #f) regular-files)]
        [else
         (define neo-files
           (for/set ([p (in-list files)])
             (build-path dir p)))
           
         (define path+items
           (for/fold ([h (hash)])
                     ([item (in-list (send parent get-items))])
             (define p (send item user-data))
             (cond
               [(set-member? neo-files p)
                (hash-set h p item)]
               [else
                (send parent delete-item item)
                h])))
           
         (for ([d (in-list dirs)])
           (unless (hash-has-key? path+items (build-path dir d))
             ((add-item! #t) d)))
           
         (for ([f (in-list regular-files)])
           (unless (hash-has-key? path+items (build-path dir f))
             ((add-item! #f) f)))]))

    (define/override (on-double-select i)
      (when i
        (cond
          [(send i compound?) (send i toggle-open/closed)]
          [else (select-callback (send i user-data))]
          )
        (send i select #t))
      )

    
    
    (define/override (on-event ev)
      (when (send ev button-down? 'left)
        (define i (get-selected))
        (when i (send i select #f))
        )
      (super on-event ev)
      (when (send ev button-up? 'right)
        (popup-menu my-popup-menu (send ev get-x)
                    (send ev get-y)))
      
      )
    (define/override (on-item-opened item)
      (send item on-opened)
      (set-add! opened (send item user-data))
      (opened-change-callback)
      (void)
      )
    (define/override (on-item-closed item)
      (send item on-closed)
      (set-remove! opened (send item user-data))
      (opened-change-callback)
      (void)
      )

    (define/public (get-opened-inside)
      (cond
        [(not the-dir) (mutable-set)]
        [else
         (define opened-inside (mutable-set the-dir))
         (define (recur item)
           (when (and (is-a? item hierarchical-list-compound-item<%>)
                      (send item is-open?))
             (set-add! opened-inside (send item user-data))
             (for-each recur (send item get-items))))
         (for-each recur (get-items))
         opened-inside]))
    ))




(module+ test1
  (define f (new frame% [label "hierlist demo"][width 400][height 500]))
  (define dl (new directory-list% [parent f]))
  (send dl set-dir! "d:/minecraft")
  (send dl update-files!)
  (send f show #t))
                               
