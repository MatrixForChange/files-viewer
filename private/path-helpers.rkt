#lang racket
(provide path-/string)
(require rackunit)
(define (path-/string p1 p2)
  (define s1 (path->string p1))
  (define s2 (path->string p2))
  (substring s1 (string-length s2)))

(module+ test
  (check-equal? "minecraft" (path-/string (string->path "d:/minecraft")
                                          (string->path "d:/"))))