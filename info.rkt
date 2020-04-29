#lang info
(define collection "files-viewer")
(define authors '("jiangqi"))
(define version "0.00.45")
(define package-content-state '(source "7.2"))
(define deps '("base" "gui-lib" "drracket" "rackunit-lib" "scheme-lib"
                      "compatibility-lib" "scribble-lib"
                      "pict-lib"))
(define drracket-tool-names (list "file-viewer"))
(define drracket-tool-icons  '(#f))

(define scribblings '(("scribblings/files-viewer.scrbl" () (tool) "files-viewer")))

(define drracket-tools (list (list "tool.rkt")))
(define pkg-desc "File manager for DrRacket")
