#lang racket

(provide/contract
 [fschange%
  (class/c [shutdown (->m void?)]
           [change-dirs (->m (set/c path-string? #:kind 'dont-care) void?)]
           [need-update?! (->m boolean?)])])

(define fschange%
  (class object%
    
    (define dirs (set))
    (define sema (make-semaphore 0))

    (define cust (make-custodian))
    (define changed (box #f))

    (parameterize ([current-custodian cust])
      (thread
       (λ ()
         (define (diff dirs+evts new-dirs)
           (define reused
             (for/fold ([h (hash)])
                       ([(k v) (in-hash dirs+evts)])
               (cond
                 [(set-member? new-dirs k) (hash-set h k v)]
                 [else 
                  (filesystem-change-evt-cancel v)
                  h])))
           (for/fold ([h reused])
                     ([d (in-set new-dirs)])
             (cond
               [(hash-ref h d (λ () #f)) h]
               [(with-handlers ([exn:fail:filesystem? (λ (e) #f)])
                  (filesystem-change-evt d (λ () #f)))
                =>
                (λ (evt) (hash-set h d evt))]
               [else h])))
         
         (let loop ([dirs+evts (diff (hash) dirs)])
           (apply
            sync
            (handle-evt sema (λ (_) (loop (diff dirs+evts dirs))))
            (for/list ([(changed-dir v) (in-hash dirs+evts)])
              (handle-evt
               v
               (λ (_)
                 (set-box! changed #t)
                 (cond
                   [(with-handlers ([exn:fail:filesystem? (λ (e) #f)])
                      (filesystem-change-evt changed-dir (λ () #f)))
                    =>
                    (λ (evt) (loop (hash-set dirs+evts changed-dir evt)))]
                   [else
                    (loop (hash-remove dirs+evts changed-dir))])))))))))

    (define/public (need-update?!)
      (box-cas! changed #t #f))

    (define/public (shutdown)
      (custodian-shutdown-all cust)
      (void))

    (define/public (change-dirs new-dirs)
      ;(log-info "~a" new-dirs)
      (set! dirs new-dirs)
      (semaphore-post sema))
    
    (super-new)))

