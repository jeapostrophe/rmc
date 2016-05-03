#lang racket/base
(require (for-syntax racket/base
                     syntax/parse)
         racket/match
         ffi/unsafe
         "cl.rkt")

(define (Type->ffi t)
  (match t
    ;; XXX Expand (and/or put into class defn?)
    [(? Int?)
     (hash-ref
      (if (Int-signed? t)
          (hasheq 8 _sint8
                  16 _sint16
                  32 _sint32
                  64 _sint64)
          (hasheq 8 _uint8
                  16 _uint16
                  32 _uint32
                  64 _uint64))
      (Int-bits t))]
    [(? Fun?)
     (_cprocedure (map Type->ffi (Fun-dom t))
                  (Type->ffi (Fun-rng t)))]))

(define (compile+link+ret is ds)
  (define n->d
    (for/hash ([i (in-list is)]
               [d (in-list ds)])
      (values (gencsym i) d)))
  (define u ($lib n->d))
  (define er (emit u))
  (define n->rt
    (for/hash ([(n d) (in-hash n->d)])
      (values n (Type->ffi ((Decl-ty d))))))
  (define vs
    (let ()
      (define the-lib
        (call-with-temporary
         "~a.lib"
         (λ (lib-p)
           (call-with-temporary
            "~a.o"
            (λ (object-p)
              (call-with-temporary
               "~a.c"
               (λ (c-p)
                 (output-c! c-p er)
                 (compile! c-p object-p er)))
              (link-lib! object-p lib-p er)))
           (ffi-lib lib-p))))
      (for/list ([(n rt) (in-hash n->rt)])
        (get-ffi-obj n the-lib rt))))
  (apply values vs))

(define-syntax (define-cl-lib stx)
  (syntax-parse stx
    [(_ [i:id d:expr] ...+)
     (syntax/loc stx
       (define-values (i ...)
         (compile+link+ret '(i ...) (list d ...))))]))

(provide define-cl-lib)
