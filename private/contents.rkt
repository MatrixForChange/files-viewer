#lang racket
(provide (all-defined-out))

(define CONTENT-GITIGNORE (string-append
                           "compiled/\n"
                           "/doc/\n"
                           "*.[0-9]\n"
                           "*.bak\n"
                           ".DS_Store\n"
                           ".\\#*\n"
                           "\\#*\n"
                           "*~\n"))

(define CONTENT-PLUGIN (string-append
                        "#lang racket/gui\n"
                        "(require drracket/tool racket/gui framework)\n"
                        "(provide tool@)\n"
                        "\n"
                        "(define tool@\n"
                        "  (unit\n"
                        "    (import drracket:tool^)\n"
                        "    (export drracket:tool-exports^)\n"
                        "    (define phase1 void)\n"
                        "    (define phase2 void)\n"
                        "    ))\n"))
