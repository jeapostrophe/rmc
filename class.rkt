#lang racket/base
(require (for-syntax racket/base
                     racket/syntax
                     syntax/parse
                     syntax/srcloc)
         syntax/location
         racket/contract/base
         racket/match)

(define-syntax (define-srcloc-struct stx)
  (syntax-parse stx
    [(_ name:id [f:id ctc:expr] ...)
     (with-syntax* ([_name (format-id #'name "_~a" #'name)]
                    [_name? (format-id #'_name "~a?" #'_name)]
                    [name? (format-id #'name "~a?" #'name)]
                    [(n ...)
                     (for/list ([f (in-list (syntax->list #'(f ...)))])
                       (generate-temporary))]
                    [(name-f ...)
                     (for/list ([f (in-list (syntax->list #'(f ...)))])
                       (format-id #'name "~a-~a" #'name f))]
                    [(_name-f ...)
                     (for/list ([f (in-list (syntax->list #'(f ...)))])
                       (format-id #'_name "~a-~a" #'_name f))])
       (syntax/loc stx
         (begin
           (struct _name (srcloc f ...))
           (define-syntax (name stx)
             (syntax-parse stx
               [(_ n ...)
                (with-syntax*
                  ([pos (syntax-source stx)]
                   [neg (syntax-source #'name)]
                   [the-srcloc #`(quote-srcloc #,stx)])
                  (syntax/loc stx
                    (let ([srcloc the-srcloc]
                          [f n]
                          ...)
                      (_name srcloc
                             (contract ctc f 'pos 'neg 'f srcloc)
                             ...))))]))
           (define name-f _name-f)
           ...
           (define name? _name?)
           (provide
            name
            (contract-out
             [_name? (-> any/c boolean?)]
             [name? (-> any/c boolean?)])))))]))

(begin-for-syntax
  (struct interface-info (stx)))

(struct object (interfaces fields))

(define-syntax (define-interface stx)
  (syntax-parse stx
    [(_ name:id [m:id ctc:expr] ...)
     (with-syntax* ([name? (format-id #'name "~a?" #'name)]
                    [inst:name (format-id #'name "inst:~a" #'name)]
                    [_inst:name (format-id #'inst:name "_~a" #'inst:name)]
                    [((_inst:name-m name-m) ...)
                     (for/list ([m (in-list (syntax->list #'(m ...)))])
                       (list (format-id #'_inst:name "~a-~a" #'_inst:name m)
                             (format-id #'name "~a-~a" #'name m)))])
       (syntax/loc stx
         (begin
           (define-srcloc-struct inst:name [m (-> object? ctc)] ...)
           (define-syntax name (interface-info #'(_inst:name ([m _inst:name-m] ...))))
           (define (name? x)
             (and (object? x)
                  (hash-has-key? (object-interfaces x) _inst:name)))
           (define (name-m x)
             (hash-ref (hash-ref (object-interfaces x) _inst:name
                                 (λ ()
                                   (error 'name-m "Not an instance of ~v: ~e"
                                          'name x)))
                       _inst:name-m
                       (λ ()
                         (error 'name-m "Interface missing method ~v: ~e"
                                'name-m x))))
           ...
           (provide
            (contract-out
             [name? (-> any/c boolean?)]
             [name-m (-> name? ctc)]
             ...)))))]))

(define-syntax (make-interface-record stx)
  (syntax-parse stx
    [(_ iname)
     #:declare iname (static interface-info? "interface")
     (with-syntax*
       ([(_inst:name ([m _inst:name-m] ...))
         (interface-info-stx (attribute iname.value))]
        [(m^ ...)
         (for/list ([m (in-list (syntax->list #'(m ...)))])
           (datum->syntax
            #'iname
            (syntax->datum m)
            #'iname))])
       (syntax/loc stx
         (cons
          _inst:name
          (make-immutable-hasheq
           (list (cons _inst:name-m m^)
                 ...)))))]))

(define-syntax (define-class stx)
  (syntax-parse stx
    [(_ name:id
        #:fields [f:id f-ctc:expr] ...
        (~optional (~seq #:procedure proc:id))
        (~seq #:methods iname:id
              idef:expr ...)
        ...)
     (with-syntax*
       ([(n ...) (generate-temporaries #'(f ...))]
        [maybe-proc
         (if (attribute proc)
             (syntax/loc stx
               (#:property prop:procedure
                (λ (me . args)
                  (proc me args))))
             #'())]
        [name? (format-id #'name "~a?" #'name)]
        [name-object (format-id #f "~a-~a" #'name #'object)]
        [name-object? (format-id #'name-object "~a?" #'name-object)]
        [(name-f ...)
         (for/list ([f (in-list (syntax->list #'(f ...)))])
           (format-id #'name "~a-~a" #'name f))]
        [name-fields (format-id #f "~a-fields" #'name)]
        [name-fields? (format-id #'name-fields "~a?" #'name-fields)]
        [_name-fields (format-id #'name-fields "_~a" #'name-fields)]
        [(_name-fields-f ...)
         (for/list ([f (in-list (syntax->list #'(f ...)))])
           (format-id #'_name-fields "~a-~a" #'_name-fields f))])
       (syntax/loc stx
         (begin
           (define (make-name-object fields)
             (match-define (_name-fields srcloc f ...) fields)
             (name-object
              (make-immutable-hasheq
               (list
                (let ()
                  idef ...
                  (make-interface-record iname))
                ...))
              fields))
           (define-srcloc-struct name-fields [f f-ctc] ...)
           (struct name-object object ()
             #:reflection-name 'name
             . maybe-proc)
           (define-syntax (name stx)
             (syntax-parse stx
               [(_ n ...)
                (quasisyntax/loc stx
                  (make-name-object
                   #,(syntax/loc stx
                       (name-fields n ...))))]
               [_:id
                (quasisyntax/loc stx
                  (make-name-object
                   #,(syntax/loc stx
                       (name-fields))))]))
           (define (name? x)
             (name-object? x))
           (define (name-f x)
             (_name-fields-f (object-fields x)))
           ...
           (provide
            name
            (contract-out
             [name? (-> any/c boolean?)]
             [name-f (-> name? any/c)]
             ...)))))]))

(define-syntax (define-class-alias stx)
  (syntax-parse stx
    [(_ name:id (real-name:id arg:expr ...)
        (~optional (~seq #:no-provide (~bind [no-provide? #t]))))
     (with-syntax
       ([maybe-provide
         (if (attribute no-provide?)
             #'()
             (syntax/loc stx ((provide name))))])
       (syntax/loc stx
         (begin
           (define-syntax (name stx)
             (syntax-parse stx
               [(_ more-arg:expr (... ...))
                (syntax/loc stx
                  (real-name arg ... more-arg (... ...)))]
               [_:id
                (syntax/loc stx
                  (real-name arg ...))]))
           . maybe-provide)))]))

(provide define-srcloc-struct
         define-interface
         define-class
         define-class-alias)
