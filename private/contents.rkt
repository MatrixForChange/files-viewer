#lang racket
(provide (all-defined-out))

(define CONTENT-GITIGNORE (string-append
                           "*.rkt\n"
                           "compiled/\n"
                           "doc/\n"
                           "*.[0-9]\n"))