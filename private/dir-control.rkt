#lang racket/base
(require racket/gui/base
         racket/class
         racket/list)
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
  (class canvas%
    (inherit refresh get-dc popup-menu set-canvas-background
              init-auto-scrollbars)
    
    (init [callback (λ (ce e)
                      (println (list-ref (send ce get-path-elements)
                                         (get-field path-index e)))
                      (flush-output))])
    (field [highlighted "orange"])
    (define path-elements '()) ; alist ordered list of ordered pairs
    (define path-index #f)
    (define mouse-pos  (new mouse-event% [event-type 'motion]))
    (define gap 15) ; the number of pixels to increase x to seperate segments
    (define left-margin 8) ; margin between left of segment and text start
    (define/public-final (get-path-elements) path-elements)
    (define/public-final (set-path _path)
      (set! path-elements _path)
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
      (send dc set-pen "black" 1 'solid)

      (for/fold ([xoffset 0]
                  )
                ([(pe i) (in-indexed path-elements)])
        (define label (car pe))
        (define (draw-background-segment
                 a-dc side-width text-height xoffset yoffset colour)

          (send dc set-brush colour 'solid)
          (define height (+ text-height (/ text-height 5)))
          (send a-dc draw-polygon
                (segment-outline-list height side-width)
                xoffset yoffset))

        (define-values (width font-height pd pa)
          (send dc get-text-extent label))
        
        (draw-background-segment
         dc (+ width 10) font-height xoffset 0 ; y offset
         (cond [(and (<= xoffset (send mouse-pos get-x) (+ xoffset width 10))
                     (<= 0 (send mouse-pos get-y) (* 1.2 font-height)))
                (set! path-index i) highlighted]
               [else "Gainsboro"]))
        (cond
          [(= xoffset 0)
           (send dc draw-text label (+ xoffset left-margin) 0)
           (+ xoffset gap width)]
          [else
           (send dc draw-text label (+ xoffset left-margin) 0)
           (+ xoffset gap width)]))
      (send dc set-brush old-brush)
      (send dc set-pen old-pen))
    
    (super-new [style '(hscroll)][stretchable-height #f][min-height 45])
    
    (set-canvas-background (make-object color% "WhiteSmoke"))
    (init-auto-scrollbars 100 #f 0.0 0.0)
    (define (select-action mouse-xpos)
      (callback this (new dir-control-event% [path-index path-index])))
    
    (define/override (on-event me)
      (send me set-y (- (send me get-y) 5))
      (case (send me get-event-type)
        [(motion) (highlight-if-hover me)
                  (refresh)]
        [(left-down) (select-action (send mouse-pos get-x))])
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
