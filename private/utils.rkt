#lang racket
;;; aux funcs.
(provide (all-defined-out))

;; NOTE: assumes `m` is a (Listof (List KeyType ValueType)).
;; this func will not set! for you; you have to do it manually.
(define (assocmap-set m k v)
  (cond
    ((null? m) m)
    ((equal? (caar m) k) (cons (list k v) (cdr m)))
    (else (cons (car m) (assocmap-set (cdr m) k v)))))

