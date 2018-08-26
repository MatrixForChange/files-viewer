#lang racket
(provide path-/string new-file-dialog delete-file-and-directory
         process/safe)
(require rackunit racket/gui syntax/parse/define (for-syntax racket/syntax)
         )

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
                      (format "Are you sure you want to delete ~a" path)
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
      (message-box "Error" "File exists, can't create!")
      (let ([p (open-output-file new-name)])
        (display content p)
        (close-output-port p))))

(define (new-file-dialog dparent current-path)
  (define file-dialog%
    (class dialog%
      (super-new
       [label "Create New File"]
       [width 430]
       [height 200]
       [parent dparent])
      (define name (new text-field% [label "Name:"]
                        [parent this]))
      (send name focus)

      (define file (new button% [label "File (Enter)"]
                        [parent this][stretchable-width #t]
                        [callback (λ (c e)
                                    (cond
                                      [(string=? (send name get-value) "")
                                       (message-box "Error" "File name is empty, can't create file.")]
                                      [(path-has-extension? (send name get-value) ".rkt")
                                       (create-new-file current-path
                                                       (send name get-value)
                                                       "#lang racket\n")]
                                      [else (create-new-file current-path
                                                       (send name get-value)
                                                       "")])
                                    (send d show #f)
                                    )]))

      (define dir (new button% [label "Directory"]
                       [parent this][stretchable-width #t]
                       [callback (λ (c e)
                                   (with-handlers
                                       ([exn:fail? (λ (e)
                                                     (message-box "Error"
                                                                  "Fail to create directory here, or your directory name is empty."))])
                                     (make-directory (build-path current-path (send name get-value))))
                                   (send d show #f))]))
      (define/override (on-subwindow-char recv ev)
        (when (equal? (send ev get-key-code) #\return)
          (send file command (make-object control-event% 'button (current-milliseconds))))
        (super on-subwindow-char recv ev))
      )
    )
  (define d (new file-dialog%))
  
  (send d show #t)
  )
       

(module+ test
  (check-equal? "minecraft" (path-/string (string->path "d:/minecraft")
                                          (string->path "d:/"))))
