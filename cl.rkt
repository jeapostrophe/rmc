#lang racket/base
;; Library
(require (for-syntax racket/base
                     racket/syntax
                     syntax/parse
                     syntax/srcloc)
         syntax/location
         racket/contract/base
         racket/generic
         racket/match)

(define-syntax (define-srcloc-struct stx)
  (syntax-parse stx
    [(_ name:id [f:id ctc:expr] ...)
     (with-syntax* ([_name (format-id #'name "_~a" #'name)]
                    [_name? (format-id #'_name "~a?" #'_name)]
                    [name? (format-id #'name "~a?" #'name)]
                    [(n ...) (generate-temporaries #'(f ...))]
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
             (hash-ref ((hash-ref (object-interfaces x) _inst:name)
                        (object-fields x))
                       _inst:name-m))
           ...
           (provide
            (contract-out
             [name? (-> any/c boolean?)]
             [name-m (-> name? ctc)]
             ...)))))]))

(define-syntax (define-class stx)
  (syntax-parse stx
    [(_ name:id
        #:fields [f:id f-ctc:expr] ...
        #:methods iname
        idef:expr ...)
     #:declare iname (static interface-info? "interface")
     (with-syntax*
       ([(n ...) (generate-temporaries #'(f ...))]
        [name? (format-id #'name "~a?" #'name)]
        [(name-f ...)
         (for/list ([f (in-list (syntax->list #'(f ...)))])
           (format-id #'name "~a-~a" #'name f))]
        [name-fields (format-id #f "~a-fields" #'name)]
        [name-fields? (format-id #'name-fields "~a?" #'name-fields)]
        [_name-fields (format-id #'name-fields "_~a" #'name-fields)]
        [(_name-fields-f ...)
         (for/list ([f (in-list (syntax->list #'(f ...)))])
           (format-id #'_name-fields "~a-~a" #'_name-fields f))]
        [(_inst:name ([m _inst:name-m] ...))
         (interface-info-stx (attribute iname.value))]
        [(m^ ...)
         (for/list ([m (in-list (syntax->list #'(m ...)))])
           (datum->syntax
            (syntax-local-introduce m)
            (syntax->datum m)
            #'iname))])
       (syntax/loc stx
         (begin
           (define name-interfaces
             (make-immutable-hasheq
              (list
               (cons
                _inst:name
                (λ (fields)
                  (match-define (_name-fields srcloc f ...) fields)
                  idef ...
                  (make-immutable-hasheq
                   (list (cons _inst:name-m m^)
                         ...)))))))
           (define-srcloc-struct name-fields [f f-ctc] ...)
           (define-syntax (name stx)
             (syntax-parse stx
               [(_ n ...)
                (quasisyntax/loc stx
                  (object name-interfaces
                          #,(syntax/loc stx
                              (name-fields n ...))))]))
           (define (name? x)
             (and (object? x)
                  (name-fields? (object-fields x))))
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
    [(_ name:id (real-name:id arg:expr ...))
     (syntax/loc stx
       (begin
         (define-syntax (name stx)
           (syntax-parse stx
             [_:id
              (syntax/loc stx
                (real-name arg ...))]))
         (provide name)))]))

;; Definitions
(require (prefix-in pp: pprint))

;; XXX make sure string is okay
(define CName? string?)

(define-srcloc-struct CHeader
  [cflags (listof string?)]
  [ldflags? (listof string?)]
  [pre (listof string?)]
  [litc string?]
  [post (listof string?)])

(define-interface Type
  [name
   (-> any/c)]
  [eq
   (-> Type?
       boolean?)]
  [pp
   (-> #:name (or/c false/c CName?) #:ptrs exact-nonnegative-integer?
       pp:doc?)]
  [headers!
   (-> (-> CHeader? void?)
       void?)])

(define (pp:ty-name t n p)
  (define tp
    (if (zero? p)
        t
        (format "~a ~a" t (make-string p #\*))))
  (if n
      (pp:hs-append (pp:text tp) (pp:text n))
      (pp:text tp)))

(define-class Literal
  #:fields
  [lit string?]
  #:methods Type
  (define (name) lit)
  (define (pp #:name n #:ptrs p)
    (pp:ty-name lit n p))
  (define (headers! !) (void))
  (define (eq t)
    (and (Literal? t)
         (equal? lit (Literal-lit t)))))

(define-class-alias Size (Literal "size_t"))
(define-class-alias Char (Literal "char"))
(define-class-alias Void (Literal "void"))

(define-class Int
  #:fields
  [signed? boolean?]
  [bits (one-of/c 8 16 32 64)]
  #:methods Type
  (define (name)
    (format "~aint~a_t" (if signed? "" "u") bits))
  (define (pp #:name n #:ptrs p)
    (pp:ty-name (name) n p))
  (define (headers! !) (void))
  (define (eq t)
    (and (Int? t)
         (eq? signed? (Int-signed? t))
         (= bits (Int-bits t)))))

(define-class-alias  UI8 (Int #f 8))
(define-class-alias UI16 (Int #f 16))
(define-class-alias UI32 (Int #f 32))
(define-class-alias UI64 (Int #f 64))

(define-class-alias  SI8 (Int #t 8))
(define-class-alias SI16 (Int #t 16))
(define-class-alias SI32 (Int #t 32))
(define-class-alias SI64 (Int #t 64))

(define-class Float
  #:fields
  [bits (one-of/c 32 64)]
  #:methods Type
  (define (name)
    (match bits
      [32 "float"]
      [64 "double"]))
  (define (pp #:name n #:ptrs p)
    (pp:ty-name (name) n p))
  (define (headers! !) (void))
  (define (eq t)
    (and (Float? t)
         (= bits (Float-bits t)))))

(define-class-alias F32 (Float 32))
(define-class-alias F64 (Float 64))

;; XXX Record Ptr Arr Union Fun
;; XXX Any Opaque Extern

(define-class Seal
  #:fields
  [tag symbol?] [st Type?]
  #:methods Type
  (define (name) (format "~a(~a)" tag ((Type-name st))))
  (define (pp #:name n #:ptrs p)
    ((Type-pp st) #:name n #:ptrs p))
  (define (headers! !)
    ((Type-headers! st) !))
  (define (eq t)
    (and (Seal? t)
         (eq? tag (Seal-tag t))
         ((Type-eq st) (Seal-st t)))))

(define-class-alias Bool (Seal 'Bool UI8))

(define-srcloc-struct Var
  [type Type?]
  [name CName?])

(define (Var-pp var)
  (λ ()
    ((Type-pp (Var-type var)) #:name (Var-name var))))

(define-interface Expr
  [pp
   (-> pp:doc?)]
  [ty
   (-> Type?)]
  [lval?
   (-> boolean?)]
  [headers!
   (-> (-> CHeader? void?)
       void?)])

(define (Expr/c ty)
  (and/c Expr?
         (flat-named-contract
          (format "~a type" ((Type-name ty)))
          (let ([eq (Type-eq ty)])
            (λ (x)
              (eq ((Expr-ty x))))))))

(define Lval/c
  (and/c Expr?
         (flat-named-contract
          "Expr-lval? returns #t"
          (λ (x)
            ((Expr-lval? x))))))

(define-interface Stmt
  [pp
   (-> pp:doc?)]
  [headers!
   (-> (-> CHeader? void?)
       void?)]
  [ret?
   (-> boolean?)])

(define NEST 2)

(define-class $nop
  #:fields
  #:methods Stmt
  (define (pp) (pp:text ";"))
  (define (headers! !) (void))
  (define (ret?) #f))

(define-class $seq
  #:fields
  [a Stmt?] [b Stmt?]
  #:methods Stmt
  (define (pp)
    (pp:v-append ((Stmt-pp a)) ((Stmt-pp b))))
  (define (headers! !)
    ((Stmt-headers! a) !)
    ((Stmt-headers! b) !))
  (define (ret?)
    ((Stmt-ret? b))))

(define-class $do
  #:fields
  [e (Expr/c Void)]
  #:methods Stmt
  (define (pp)
    (pp:h-append (pp:text "(void)") ((Expr-pp e)) pp:semi))
  (define (headers! !)
    ((Expr-headers! e) !))
  (define (ret?)
    #f))

(define-class $if
  #:fields
  [test (Expr/c Bool)]
  [ift Stmt?]
  [iff Stmt?]
  #:methods Stmt
  (define (pp)
    (pp:h-append (pp:hs-append (pp:text "if")
                               pp:lparen ((Expr-pp test)) pp:rparen
                               pp:lbrace)
                 (pp:nest NEST
                          (pp:h-append
                           pp:line ((Stmt-pp ift))))
                 pp:line
                 (pp:nest NEST
                          (pp:h-append
                           (pp:hs-append pp:rbrace (pp:text "else") pp:lbrace) pp:line
                           ((Stmt-pp iff))))
                 pp:line
                 pp:rbrace))
  (define (headers! !)
    ((Expr-headers! test) !)
    ((Stmt-headers! ift) !)
    ((Stmt-headers! iff) !))
  (define (ret?)
    (and ((Stmt-ret? ift))
         ((Stmt-ret? iff)))))

(define-class $%while
  #:fields
  [test (Expr/c Bool)]
  [body Stmt?]
  #:methods Stmt
  (define (pp)
    (pp:h-append (pp:hs-append (pp:text "while")
                               pp:lparen ((Expr-pp test)) pp:rparen
                               pp:lbrace )
                 (pp:nest NEST
                          (pp:h-append
                           pp:line
                           ((Stmt-pp body))))
                 pp:line
                 pp:rbrace))
  (define (headers! !)
    ((Expr-headers! test) !)
    ((Stmt-headers! body) !))
  (define (ret?)
    ((Stmt-ret? body))))

(define-class $%let1
  #:fields
  [var Var?]
  [body Stmt?]
  #:methods Stmt
  (define (pp)
    (pp:h-append pp:lbrace pp:space ((Var-pp var)) pp:semi
                 (pp:nest NEST (pp:h-append pp:line ((Stmt-pp body)))) pp:rbrace))
  (define (headers! !)
    ((Stmt-headers! body) !))
  (define (ret?)
    ((Stmt-ret? body))))

(define-class $set!
  #:fields
  [lval Lval/c]
  [rhs (Expr/c ((Expr-ty lval)))]
  #:methods Stmt
  (define (pp)
    (pp:h-append ((Expr-pp lval))
                 pp:space (pp:char #\=) pp:space
                 ((Expr-pp rhs)) pp:semi))
  (define (headers! !)
    ((Expr-headers! lval) !)
    ((Expr-headers! rhs) !))
  (define (ret?)
    #f))

(define-class $ret/ty
  #:fields
  [ty Type?]
  [e (Expr/c ty)]
  #:methods
  (define (pp)
    (pp:h-append (pp:text "return") pp:space ((Expr-pp e)) pp:semi))
  (define (headers! !)
    ((Expr-headers! e)))
  (define (ret?) #t))

(define-class $ret/Void
  #:fields
  #:methods
  (define (pp)
    (pp:h-append (pp:text "return") pp:semi))
  (define (headers! !) (void))
  (define (ret?) #t))

(module* test racket/base
  (require (submod ".."))
  (define (ppt . ds)
    (for-each (λ (x) (pp:pretty-print (pp:h-append x pp:line))) ds))
  (ppt
   ((Type-pp UI8) #:name #f #:ptrs 0)
   ((Type-pp UI8) #:name #f #:ptrs 1)
   ((Type-pp UI8) #:name #f #:ptrs 2)
   ((Type-pp UI8) #:name "example" #:ptrs 0)
   ((Type-pp UI8) #:name "example" #:ptrs 1)
   ((Type-pp UI8) #:name "example" #:ptrs 2))
  ($do 5))
