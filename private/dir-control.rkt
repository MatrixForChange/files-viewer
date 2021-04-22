#lang racket/base

;; (c) 2018 Stephen De Gabrielle
;; original at 
;; https://github.com/spdegabrielle/dir-control/edit/master/dir-control.rkt

(require racket/gui/base
         racket/class
         racket/list
         "color-scheme.rkt")
(provide dir-control% path-alist)
;;

(define (my-directory-list dir #:hidden [hidden #t])
  (if (not hidden)
      (filter (λ (p) (if (equal? (string-ref (path->string p) 0) #\.) #f #t))
              (directory-list dir))
      (directory-list dir)))

;; list the full paths above this one
(define (parent-paths path)
  (define-values (base name dir) (split-path path))
  (cond
    [(equal? base #f) (list path)]
    [else (cons path (parent-paths base))]))
;; create an alist of (string . path) for each path above path
(define (path-alist path)
  (reverse (map  (λ (p) (cons (path->string (last (explode-path p))) p)) (parent-paths path))))

(define dir-control-event%
  (class event%
    (init-field [path-index #f])
    (super-new)))

(define dir-control%
  (class (color-scheme-mixin canvas%)
    (inherit refresh get-dc popup-menu set-canvas-background
              init-auto-scrollbars get-view-start
              get-selected-background get-text-foreground)
    
    (init [callback (λ (ce e)
                      (println (list-ref (send ce get-path-elements)
                                         (get-field path-index e)))
                      (flush-output))])
    (init-field [highlighted "orange"])
    (define need-resize? #t)
    (define path-elements '()) ; alist ordered list of ordered pairs
    (define path-index #f)
    (define mouse-pos  (new mouse-event% [event-type 'motion]))
    (define gap 15) ; the number of pixels to increase x to seperate segments
    (define left-margin 8) ; margin between left of segment and text start
    (define/public-final (get-path-elements) path-elements)
    (define/public-final (set-path _path)
      (set! path-elements _path)
      (set! need-resize? #t)
      (refresh))
    

    (define (highlight-if-hover v) (set! mouse-pos v))
    ;; segment-outline-list : height side indent -> listof point
    (define (segment-outline-list height side [indent (/ height 3)])
      `((               0 . 0)
        (           ,side . 0)
        (,(+ side indent) . ,(/ height 2))
        (           ,side . ,height)
        (               0 . ,height)
        (         ,indent . ,(/ height 2))))
    (define/override (on-paint)
      (define dc (get-dc))
      (send dc set-smoothing 'aligned)
      (send dc set-origin 0 5)
      (define old-brush (send dc get-brush))
      (define old-pen (send dc get-pen))

      (send dc set-brush "silver" 'solid)

      (for/fold ([xoffset 0]
                 #:result (when need-resize?
                            (init-auto-scrollbars
                             (inexact->exact (+ gap xoffset))
                             1 0.0 0.0)
                            (set! need-resize? #f))
                  )
                ([(pe i) (in-indexed path-elements)])
        (define label (car pe))
        (define (draw-background-segment
                 a-dc side-width text-height xoffset yoffset highlight?)
          (cond
            [highlight?
             (send dc set-pen (get-highlight-text-color) 1 'solid)
             (send dc set-brush (get-highlight-background-color) 'solid)]
            [else
             (send dc set-pen (get-text-foreground) 1 'solid)
             (send dc set-brush (get-selected-background) 'solid)])
          (define height (+ text-height (/ text-height 5)))
          (send a-dc draw-polygon
                (segment-outline-list height side-width)
                xoffset yoffset))

        (define-values (width font-height pd pa)
          (send dc get-text-extent label))

        (define highlight?
          (and (<= xoffset (send mouse-pos get-x) (+ xoffset width 10))
               (<= 0 (send mouse-pos get-y) (* 1.2 font-height))))
        
        (draw-background-segment
         dc (+ width 10) font-height xoffset 0 ; y offset
         highlight?)

        (when highlight?
          (set! path-index i))
        
        (send dc set-text-foreground
              (if highlight?
                  (get-highlight-text-color)
                  (get-text-foreground)))
        (send dc draw-text label (+ xoffset left-margin) 0)
        (+ xoffset gap width))

      (send dc set-brush old-brush)
      (send dc set-pen old-pen))
    
    (super-new [style '(hscroll)][stretchable-height #f][min-height 45])
    
    (define (select-action mouse-xpos)
      (callback this (new dir-control-event% [path-index path-index])))
    
    (define/override (on-event me)
      (send me set-y (- (send me get-y) 5))
      (define-values (a b) (get-view-start))
      (send me set-x (+ a (send me get-x)))
      (case (send me get-event-type)
        [(motion) (highlight-if-hover me)
                  (refresh)]
        [(left-down) (select-action (send mouse-pos get-x))
                     (set! need-resize? #t)
                     (refresh)])
      (super on-event me))

    (send (get-dc) set-font normal-control-font)))


(module+ test1
  (define f (new frame% [width 400] [height 100] [label ""]))
  (define dir-control (new dir-control%
                           [parent f]
                           [callback (λ (ce e)
                                       (println (list-ref (send ce get-path-elements)
                                                          (get-field path-index e)))
                                       (flush-output))]))
  (define pp (new horizontal-panel% [parent f]))
  (send dir-control set-path (path-alist (current-directory-for-user)))
  (send f show #t))
