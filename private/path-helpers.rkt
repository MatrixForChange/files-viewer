#lang racket
(provide path-/string new-file-dialog delete-file-and-directory
         process/safe)
(require rackunit racket/gui syntax/parse/define (for-syntax racket/syntax)
         "content.rkt")

(define (process/safe cmd)
  (match-define (list in1 out _ in2 _) (process cmd))
  (close-input-port in1)
  (close-input-port in2)
  (close-output-port out))

(define (path-/string p1 p2)
  (define s1 (path->string p1))
  (define s2 (path->string p2))
  (substring s1 (string-length s2)))

(define (delete-file-and-directory dparent path)
  (match (message-box "File Manager"
                      (format "Are you sure to delete ~a" path)
                      dparent '(ok-cancel))
    ['ok (delete-file-and-directory/recur path)]
    ['cancel (void)]))

(define (delete-file-and-directory/recur path)
  (if (file-exists? path) (delete-file path)
      (begin
        (for ([i (in-directory path)])
          (delete-file-and-directory/recur i))
        (delete-directory path))
      ))

(define (create-new-file path name content)
  (define new-name (if (file-exists? path)
                       (simplify-path (build-path path 'up name))
                       (build-path path name)))
  (if (file-exists? new-name)
      (message-box "error" "File exists,can't create!")
      (let ([p (open-output-file new-name)])
        (display content p)
        (close-output-port p))))

(define (new-file-dialog dparent current-path)
  (define d (new dialog% [label "Create New File"]
                 [width 430]
                 [height 200]
                 [parent dparent]))
  (define name (new text-field% [label "File Name(without Suffix):"]
                    [parent d]))
  (send name focus)
  (define-simple-macro (define-file-kind kind-name desc:str suffix:str)
    #:with content-name (format-id #'here "~a-content" #'kind-name)
    (define kind-name (new button% [label desc]
                           [parent d] [stretchable-width #t]
                           [callback (λ (c e)
                                       (create-new-file current-path
                                                        (string-append (send name get-value) suffix)
                                                        content-name)
                                       (send d show #f)
                                       )])))

  (define dir (new button% [label "Directory"]
                   [parent d][stretchable-width #t]
                   [callback (λ (c e)
                               (with-handlers
                                   ([exn:fail? (λ (e)
                                                 (message-box "error"
                                                              "fail to create directory here,or your directory name is empty"))])
                                 (make-directory (build-path current-path (send name get-value))))
                               (send d show #f))]))
  
  (define-file-kind rkt-file "Racket Programs (*.rkt)" ".rkt")
  (define-file-kind rkt-gui-file "Racket GUI Programs (*.rkt)" ".rkt")
  (define-file-kind rkt-macro-file "Racket Programs with Macros (*.rkt)" ".rkt")
  (define-file-kind markdown-file "Markdown Files (*.md)" ".md")
  (define other (new button% [label "Other Files(with Suffix)"]
                     [parent d][stretchable-width #t]
                     [callback (λ (c e)
                                 (if (string=? (send name get-value) "")
                                     (message-box "error" "file name is empty,can't create file")
                                     (create-new-file current-path
                                                      (send name get-value)
                                                      ""))
                                 (send d show #f)
                                 )]))
  (send d show #t)
  )
       

(module+ test
  (check-equal? "minecraft" (path-/string (string->path "d:/minecraft")
                                          (string->path "d:/"))))