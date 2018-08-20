#lang info
(define collection "files-viewer")
(define authors '("jiangqi"))
(define version "0.00.42")
(define package-content-state '(built "7.0"))
(define deps '("base" "gui-lib" "drracket" "rackunit-lib" "scheme-lib"
                      "compatibility-lib" "scribble-lib"
                      "pict-lib"))
(define drracket-tool-names (list "file-viewer"))
(define drracket-tool-icons  '(#f))

(define scribblings '(("scribblings/files-viewer.scrbl" () (tool) "files-viewer")))

(define drracket-tools (list (list "tool.rkt")))
(define pkg-desc "file manager for DrRacket")
