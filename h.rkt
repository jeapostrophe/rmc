#lang racket/base
(require (for-syntax racket/base
                     racket/syntax
                     syntax/parse)
         racket/stxparam
         racket/splicing
         rmc)

(define (CHeader* litc
                  #:cflags [c '()]
                  #:ldflags [l '()]
                  #:pre [pre '()]
                  #:post [post '()])
  (CHeader c l pre litc post))

(define-syntax-parameter current-rmc-h-id #f)
(begin-for-syntax
  (define (read-rmc-h)
    (syntax-local-introduce (syntax-parameter-value #'current-rmc-h-id))))
(define-syntax (current-rmc-h stx)
  (or (read-rmc-h)
      (raise-syntax-error #f "illegal outside define-rmc/header" stx)))

(define-syntax (define-rmc/header stx)
  (syntax-parse stx
    [(_ body:expr ...+)
     (with-syntax ([h-id (generate-temporary)])
       (syntax/loc stx
         (splicing-syntax-parameterize
             ([current-rmc-h-id #'h-id])
           body ...)))]))

(define-syntax (H stx)
  (syntax-parse stx
    [(_ arg ...)
     (with-syntax ([h-id (read-rmc-h)])
       (syntax/loc stx
         (define h-id (CHeader* arg ...))))]))

(define-syntax (T stx)
  (syntax-parse stx
    [(_ n:id)
     (with-syntax ([n* (format-id #'n "~a*" #'n)])
       (syntax/loc stx
         (begin (define n (Extern current-rmc-h (Opaque (symbol->string 'n))))
                (define n* (Ptr n))
                (provide n n*))))]))

(begin-for-syntax
  (define-syntax-class nspec
    #:attributes (rid cstr)
    (pattern rid:id
             #:attr cstr #'(symbol->string 'rid))
    (pattern (rid:id cid:id)
             #:attr cstr #'(symbol->string 'cid))))

(define-syntax (V stx)
  (syntax-parse stx
    [(_ n)
     (syntax/loc stx
       (V n Any))]
    [(_ n:nspec t:expr)
     (syntax/loc stx
       (begin (define n.rid ($extern current-rmc-h n.cstr t))
              (provide n.rid)))]))

(define-syntax (F stx)
  (syntax-parse stx
    #:datum-literals (->)
    [(_ n dom:expr ... -> rng:expr)
     (syntax/loc stx
       (V n (Fun (list dom ...) rng)))]))

(provide define-rmc/header
         H V T F)
