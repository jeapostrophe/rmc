#lang racket/base
(require (for-syntax racket/base
                     racket/syntax
                     syntax/parse)
         syntax/parse/define
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

(define-simple-macro (Vs (n ...) t)
  (begin (V n t) ...))

(define-syntax (F stx)
  (syntax-parse stx
    #:datum-literals (->)
    [(_ n dom:expr ... -> rng:expr)
     (syntax/loc stx
       (V n (Fun (list dom ...) rng)))]))

(define-syntax (E stx)
  (syntax-parse stx
    [(_ n:id t:expr v:id ...)
     (syntax/loc stx
       (begin
         (define n-seal (gensym 'n))
         (define n (Extern current-rmc-h (Seal n-seal t)))
         (define v ($extern current-rmc-h (symbol->string 'v) n))
         ...
         (provide n v ...)))]))

(define-syntax (S stx)
  (syntax-parse stx
    [(_ n:id [t:expr f:expr] ...)
     (with-syntax ([n* (format-id #'n "~a*" #'n)])
       (syntax/loc stx
         (begin
           (define n
             (ExternStruct
              current-rmc-h (symbol->string 'n)
              [t f] ...))
           (define n* (Ptr n))
           (provide n n*))))]))

(provide define-rmc/header
         H V Vs T F E S)

(require racket/string
         racket/list
         racket/contract/base)

(define (pkg-config . args)
  (string-split
   (apply capture-subprocess
          (find-executable-path "pkg-config") args)))
(define (pkg-config-cflags lib)
  (append*
   (for/list ([l (in-list (pkg-config "--cflags" lib))])
     (if (regexp-match #rx"^-I" l)
         (list "-isystem" (substring l 2))
         (list l)))))
(define (pkg-config-ldflags lib) (pkg-config "--static" "--libs" lib))

(provide
 (contract-out
  [pkg-config-cflags (-> string? (listof string?))]
  [pkg-config-ldflags (-> string? (listof string?))]))
