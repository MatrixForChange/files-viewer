#lang racket/base

(require racket/gui/base racket/class framework)
(provide (all-defined-out))


(define canvas:color-scheme<%>
  (interface ()
    get-selected-background
    get-text-foreground))

(define-syntax-rule (define-scheme-callback pref)
  (begin
    (define callbacks (make-weak-hasheq))
    (define (pref this fn)
      (hash-set! callbacks this fn))
    (color-prefs:register-color-scheme-entry-change-callback
     'pref
     (λ (x) (for ([fn (in-hash-values callbacks)])
              (fn x)))
     ;the fn is contracted, so weak? doesn't work
     #f))
  )

(define-scheme-callback framework:basic-canvas-background)
(define-scheme-callback framework:default-text-color)
(define-scheme-callback framework:paren-match-color)

(define color-scheme-mixin
  (mixin (canvas<%>) (canvas:color-scheme<%>)
    (inherit set-canvas-background get-dc refresh)

    (define selected-background #f)
    (define text-foreground #f)
    (define/public (get-text-foreground)
      (or text-foreground (send (get-dc) get-text-foreground)))

    (define/public (get-selected-background)
      selected-background)
    
    (define-syntax-rule (register-scheme-callback (pref v) body ...)
      (begin
        (define (callback v)
          body ...)
        (define (fn x)
          (callback x)
          (refresh))
        (callback (color-prefs:lookup-in-color-scheme 'pref))
        (pref this fn))
      )
    
    (super-new)
    
    (register-scheme-callback (framework:basic-canvas-background v)
      (set-canvas-background v))
    (register-scheme-callback (framework:default-text-color v)
      (set! text-foreground v))
    (register-scheme-callback (framework:paren-match-color v)
      (set! selected-background v))
    ))