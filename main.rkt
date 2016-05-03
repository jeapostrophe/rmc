#lang racket/base
;; Library
(require (for-syntax racket/base
                     racket/syntax
                     syntax/parse
                     syntax/srcloc)
         syntax/location
         racket/contract/base
         racket/contract/combinator
         racket/match
         racket/list
         racket/bool
         syntax/parse/define
         "private/class.rkt")

;;; General
(require (prefix-in pp: pprint))

(define (CName? x)
  (and (string? x)
       (regexp-match #rx"^[_a-zA-Z][_a-zA-Z0-9]*$" x)))

(define field? symbol?)
(define field=? symbol=?)
(define field->string symbol->string)
(define (keyword->field k)
  (string->symbol (keyword->string k)))
(define (pp:field f) (pp:text (field->string f)))

(define-srcloc-struct CHeader
  [cflags (listof string?)]
  [ldflags (listof string?)]
  [pre (listof string?)]
  [litc string?]
  [post (listof string?)])

(define (pp:header h)
  (match-define (_CHeader _ _ _ pre litc post) h)
  (pp:v-append (apply pp:v-append (map pp:text pre))
               (pp:hs-append (pp:text "#include") (pp:text litc))
               (apply pp:v-append (map pp:text post))))

;;; Values
(define the-NULL (gensym))

;;; Types

(define-interface Type
  [name
   (-> any/c)]
  [eq
   (-> Type?
       boolean?)]
  [pp
   (-> #:name (or/c false/c CName?) #:ptrs exact-nonnegative-integer?
       pp:doc?)]
  [h!
   (-> (-> CHeader? void?)
       void?)]
  [val?
   (-> any/c
       boolean?)]
  [pp:val
   (-> any/c
       pp:doc?)]
  [pp:init
   (-> (or/c #f pp:doc?))])

(define (Type*-eq x y)
  (cond
    [(Extern? x)
     (Type*-eq (Extern-st x) y)]
    [(Extern? y)
     (Type*-eq x (Extern-st y))]
    [else
     (or (Any? x)
         (Any? y)
         (eq? x y)
         ((Type-eq x) y))]))
(define ((Type*-eq-p x) y)
  (Type*-eq x y))

(define-class Any
  #:fields
  #:methods Type
  (define (name) "any")
  (define (eq t) #t)
  (define (pp #:name n #:ptrs p)
    (error 'Any "Type cannot be printed"))
  (define (h! !) (void))
  (define (val? x) #f)
  (define (pp:val x)
    (error 'Any "Values cannot be printed"))
  (define (pp:init) #f))

(define (pp:ty-name t n p)
  (define tp
    (if (zero? p)
        t
        (format "~a ~a" t (make-string p #\*))))
  (if n
      (pp:hs-append (pp:text tp) (pp:text n))
      (pp:text tp)))

(define (pp:never v)
  (error 'pp:never "Cannot be printed"))

(define-class Literal
  #:fields
  [v? (-> any/c boolean?)]
  [ppv (-> any/c pp:doc?)]
  [ppi (or/c false/c pp:doc?)]
  [lit string?]
  #:methods Type
  (define (name) lit)
  (define (pp #:name n #:ptrs p)
    (pp:ty-name lit n p))
  (define (h! !) (void))
  (define (eq t)
    (and (Literal? t)
         (equal? lit (Literal-lit t))))
  (define (val? x) (v? x))
  (define (pp:val v) (ppv v))
  (define (pp:init) ppi))

(define (pp:char-val c)
  (pp:text (number->string (char->integer c))))
(define (never? x) #f)

(define-class-alias Char (Literal char? pp:char-val (pp:text "0") "char"))

(define-class-alias Opaque (Literal never? pp:never #f))

(define-class-alias Void (Opaque "void"))
(define Void? (Type*-eq-p Void))

(define-class Int
  #:fields
  [signed? boolean?]
  [bits (one-of/c 8 16 32 64)]
  #:methods Type
  (define (name)
    (format "~aint~a_t" (if signed? "" "u") bits))
  (define (pp #:name n #:ptrs p)
    (pp:ty-name (name) n p))
  (define (h! !) (void))
  (define (eq t)
    (and (Int? t)
         (eq? signed? (Int-signed? t))
         (= bits (Int-bits t))))
  (define (val? x)
    (and (exact-integer? x)
         (or signed?
             (not (negative? x)))
         (<= (integer-length x)
             (- bits (if signed? 1 0)))))
  (define (pp:val v)
    (pp:text (number->string v)))
  (define (pp:init)
    (pp:text "0")))

(define (Int-unsigned? t)
  (not (Int-signed? t)))

(define-class-alias  U8 (Int #f 8))
(define-class-alias U16 (Int #f 16))
(define-class-alias U32 (Int #f 32))
(define-class-alias U64 (Int #f 64))

(define-class-alias Size (U64)
  #; (Literal "size_t" never? pp:never))

(define-class-alias  S8 (Int #t 8))
(define-class-alias S16 (Int #t 16))
(define-class-alias S32 (Int #t 32))
(define-class-alias S64 (Int #t 64))

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
  (define (h! !) (void))
  (define (eq t)
    (and (Float? t)
         (= bits (Float-bits t))))
  (define (val? x)
    ((match bits
       [32 single-flonum?]
       [64 double-flonum?])
     x))
  (define (pp:val v)
    (pp:h-append (pp:text
                  (regexp-replace #rx"f" (number->string v) "e"))
                 (match bits
                   [32 (pp:text "f")]
                   [64 pp:empty])))
  (define (pp:init)
    (pp:h-append (pp:text "0.0")
                 (match bits
                   [32 (pp:text "f")]
                   [64 pp:empty]))))

(define-class-alias F32 (Float 32))
(define-class-alias F64 (Float 64))

(define (Numeric? x) (or (Int? x) (Float? x)))

(define-class *Ptr
  #:fields
  [k (or/c false/c exact-nonnegative-integer?)]
  [st Type?]
  #:methods Type
  (define (name)
    (define stn ((Type-name st)))
    (if k
        (format "~a[~a]" stn k)
        (format "ptr(~a)" stn)))
  (define (pp #:name n #:ptrs p)
    (if k
        (pp:h-append ((Type-pp st) #:name #f #:ptrs p)
                     (if n (pp:h-append pp:space (pp:text n)) pp:empty)
                     pp:lbracket (pp:text (number->string k)) pp:rbracket)
        ((Type-pp st) #:name n #:ptrs (add1 p))))
  (define (h! !) ((Type-h! st) !))
  (define (eq t)
    (and (*Ptr? t)
         (Type*-eq st (*Ptr-st t))
         (or (not k)
             (<= k (*Ptr-k t)))))
  (define (val? x)
    (if k
        #f
        (eq? the-NULL x)))
  (define (pp:val v)
    (if k
        (error 'pp:never)
        (pp:text "NULL")))
  (define (pp:init)
    (if k
        (pp:text "{0}")
        (pp:text "NULL"))))

(define-class-alias Ptr (*Ptr #f))
(define-class-alias Arr (*Ptr))
(define (Array? x)
  (and (*Ptr? x)
       (*Ptr-k x)))

(define-class *Struct
  #:fields
  [hn symbol?]
  [union? boolean?]
  [fs (and/c (listof (cons/c field? (-> Type?)))
             (flat-named-contract
              "unique fields"
              (λ (x)
                (define fs (map car x))
                (not (check-duplicates fs field=?)))))]
  #:methods Type
  (define cn (gencsym hn))
  (define (name)
    (list (if union? 'union 'struct)
          cn
          (for/list ([f (in-list fs)])
            (cons (car f)
                  ;; ((Type-name ((cdr f))))
                  ((cdr f))))))
  (define (eq t)
    (and (*Struct? t)
         (eq? union? (*Struct-union? t))
         (for/and ([f (in-list fs)]
                   [tf (in-list (*Struct-fs t))])
           (and (field=? (car f) (car tf))
                (Type*-eq ((cdr f)) ((cdr tf)))))))

  (define kind-pp
    (if union?
        (pp:text "union")
        (pp:text "struct")))
  (define cn-pp (pp:text cn))

  (define (pp #:name n #:ptrs p)
    (pp:h-append
     kind-pp pp:space cn-pp
     (if (zero? p)
         pp:empty
         (pp:h-append pp:space (pp:text (make-string p #\*))))
     (if n
         (pp:h-append pp:space (pp:text n))
         pp:empty)))

  (define (h! !)
    (define forward-decl-pp
      (pp:h-append kind-pp pp:space cn-pp pp:semi))
    (define decl-pp
      (pp:h-append
       kind-pp pp:space cn-pp pp:space
       pp:lbrace
       (pp:nest NEST
                (pp:h-append pp:line
                             (apply pp:v-append
                                    (for/list ([f (in-list fs)])
                                      (pp:h-append
                                       ((Type-pp ((cdr f)))
                                        #:name (field->string (car f))
                                        #:ptrs 0)
                                       pp:semi)))))
       pp:line
       pp:rbrace pp:semi))

    (when (ec-struct! this forward-decl-pp decl-pp)
      (for ([f (in-list fs)])
        ((Type-h! ((cdr f))) !))))

  (define (val? x) #f)
  (define pp:val pp:never)

  (define (pp:init)
    (cond
      [union?
       (pp:text "{0}")]
      [else
       (let/ec esc
         (pp:hs-append
          pp:lbrace
          (apply pp:hs-append
                 (pp:apply-infix
                  pp:comma
                  (for/list ([f (in-list fs)])
                    (or ((Type-pp:init ((cdr f))))
                        (esc #f)))))
          pp:rbrace))])))

(define (StructField/c ty)
  (λ (f)
    (*struct-field-ty ty f)))

(define (*struct-field-ty t f)
  (cond
    [(Any? t)
     Any]
    [else
     (for/or ([tf (*Struct-fs t)]
              #:when (field=? (car tf) f))
       ((cdr tf)))]))

(define (*struct-expr-field-ty e f)
  (*struct-field-ty ((Expr-ty e)) f))

(define-syntax (Struct stx)
  (syntax-parse stx
    [(_ [f:id ty:expr] ...)
     (quasisyntax/loc stx
       (*Struct (or '#,(syntax-local-name) (gensym 'struct))
                #f
                (list (cons 'f (λ () ty)) ...)))]))
(provide Struct)

(define-syntax (Union stx)
  (syntax-parse stx
    [(_ [f:id ty:expr] ...)
     (quasisyntax/loc stx
       (*Struct (or '#,(syntax-local-name) (gensym 'union))
                #t
                (list (cons 'f (λ () ty)) ...)))]))
(provide Union)

(define-class Fun
  #:fields
  [dom (listof Type?)]
  [rng Type?]
  #:methods Type
  (define (name)
    (list (for/list ([t (in-list dom)])
            ((Type-name t)))
          '->
          ((Type-name rng))))
  (define (eq t)
    (and (Fun? t)
         (let ([tdom (Fun-dom t)])
           (and (= (length dom) (length tdom))
                (for/and ([t (in-list dom)]
                          [s (in-list tdom)])
                  (Type*-eq t s))))
         (Type*-eq (Fun-rng t) rng)))
  (define (pp #:name n #:ptrs p)
    (pp:h-append
     ((Type-pp rng) #:name #f #:ptrs 0)
     pp:space
     pp:lparen (pp:ty-name "" n p) pp:rparen
     pp:lparen
     (apply pp:hs-append
            (pp:apply-infix pp:comma
                            (for/list ([d (in-list dom)])
                              ((Type-pp d) #:name #f #:ptrs 0))))
     pp:rparen))
  (define (h! !)
    ((Type-h! rng) !)
    (for ([d (in-list dom)])
      ((Type-h! d) !)))
  (define (val? x) #f)
  (define pp:val pp:never)
  (define (pp:init) #f))

(define-class Extern
  #:fields
  [h CHeader?]
  [st Type?]
  #:methods Type
  (define (name) ((Type-name st)))
  (define (pp #:name n #:ptrs p)
    ((Type-pp st) #:name n #:ptrs p))
  (define (h! !)
    (! h)
    ((Type-h! st) !))
  (define (eq t)
    (Type*-eq st t))
  (define (val? x)
    ((Type-val? st) x))
  (define (pp:val v)
    ((Type-pp:val st) v))
  (define (pp:init)
    ((Type-pp:init st))))

(define-class Seal*
  #:fields
  [v? (or/c #f (-> any/c boolean?))]
  [ppv (or/c #f (-> any/c pp:doc?))]
  [tag symbol?] [st Type?]
  #:methods Type
  (define (name) (format "~a<~a>" tag ((Type-name st))))
  (define (pp #:name n #:ptrs p)
    ((Type-pp st) #:name n #:ptrs p))
  (define (h! !)
    ((Type-h! st) !))
  (define (eq t)
    (and (Seal*? t)
         (eq? tag (Seal*-tag t))
         (Type*-eq st (Seal*-st t))))
  (define (val? x)
    (if v?
        (v? x)
        ((Type-val? st) x)))
  (define (pp:val v)
    (if ppv
        (ppv v)
        ((Type-pp:val st) v)))
  (define (pp:init)
    ((Type-pp:init st))))

(define-class-alias Seal (Seal* #f #f))

(define (pp:bool v)
  (pp:text (number->string (if v 1 0))))
(define-class-alias Bool (Seal* boolean? pp:bool 'Bool U8))

(define (pp:cstring v)
  (pp:text (format "~v" v)))
(define-class-alias String (Seal* string? pp:cstring 'String (Ptr Char)))

(define (gencsym [s 'c])
  (symbol->string (gensym (regexp-replace* #rx"[^A-Za-z_0-9]" (format "_~a" s) "_"))))
(provide gencsym)

;;; Expressions

(define-interface Expr
  [pp
   (-> pp:doc?)]
  [ty
   (-> Type?)]
  [lval?
   (-> boolean?)]
  [h!
   (-> (-> CHeader? void?)
       void?)])

(define (Expr/c ty)
  (Expr?/c
   (format "~a type" ((Type-name ty)))
   (Type*-eq-p ty)))

(define (Expr?/c lab ty?)
  (define name (format "Expr with ~a" lab))
  (make-flat-contract
   #:name name
   #:first-order
   (λ (x)
     (and (Expr? x)
          (let ([xt ((Expr-ty x))])
            (or (Any? xt) (ty? xt)))))
   #:projection
   (λ (b)
     (λ (x)
       (if (Expr? x)
           (let ([xt ((Expr-ty x))])
             (if (or (Any? xt) (ty? xt))
                 x
                 (raise-blame-error
                  b x
                  '(expected: "~a" given: "Expr(~e) with type ~a")
                  name x ((Type-name xt)))))
           (raise-blame-error
            b x
            '(expected: "~a" given: "~e")
            name x))))))

(define Lval/c
  (and/c Expr?
         (flat-named-contract "Expr-lval?"
                              (λ (x)
                                ((Expr-lval? x))))))

(define-class $offsetof
  #:fields
  [arg-ty (or/c Any? *Struct?)]
  [f (StructField/c arg-ty)]
  #:methods Expr
  (define (pp)
    (pp:h-append pp:lparen (pp:text "offsetof") pp:lparen
                 ((Type-pp arg-ty) #:name #f #:ptrs 0) pp:comma
                 (pp:field f) pp:rparen pp:rparen))
  (define (ty) Size)
  (define (lval?) #f)
  (define (h! !) ((Type-h! arg-ty) !)))

(define-class $fref
  #:fields
  [e (Expr?/c "struct or union" *Struct?)]
  [f (StructField/c ((Expr-ty e)))]
  #:methods Expr
  (define (pp)
    (pp:h-append pp:lparen ((Expr-pp e))
                 (pp:char #\.) (pp:field f) pp:rparen))
  (define (ty) (*struct-expr-field-ty e f))
  (define (lval?) #t)
  (define (h! !) ((Expr-h! e) !)))

(define-class $sizeof
  #:fields
  [arg-ty (and/c Type? (not/c (or/c Any? Void? Fun?)))]
  #:methods Expr
  (define (pp)
    (pp:h-append pp:lparen (pp:text "sizeof") pp:lparen
                 ((Type-pp arg-ty) #:name #f #:ptrs 0)
                 pp:rparen pp:rparen))
  (define (ty) Size)
  (define (lval?) #f)
  (define (h! !) ((Type-h! arg-ty) !)))

(define-class $aref
  #:fields
  [e (Expr?/c "array or pointer" *Ptr?)]
  [o Unsigned-Int/c]
  #:methods Expr
  (define (pp)
    (pp:h-append ((Expr-pp e)) pp:lbracket ((Expr-pp o)) pp:rbracket))
  (define (ty) (*Ptr-st ((Expr-ty e))))
  (define (lval?) #t)
  (define (h! !)
    ((Expr-h! e) !)
    ((Expr-h! o) !)))

(define-class $&
  #:fields
  [e Lval/c]
  #:methods Expr
  (define (pp) (pp:op1 "&" e))
  (define (ty) (Ptr ((Expr-ty e))))
  (define (lval?) #f)
  (define (h! !) ((Expr-h! e) !)))

(define-class $pref
  #:fields
  [e (Expr?/c "array pointer" *Ptr?)]
  #:methods Expr
  (define (pp) (pp:op1 "*" e))
  (define (ty)
    (define e-ty ((Expr-ty e)))
    (*Ptr-st e-ty))
  (define (lval?) #t)
  (define (h! !) ((Expr-h! e) !)))

(define-syntax ($@ stx)
  (syntax-parse stx
    [(_ e:expr)
     (syntax/loc stx
       e)]
    [(_ (~datum *) e:expr)
     (syntax/loc stx
       ($pref e))]
    [(_ e ... (~datum ->))
     (syntax/loc stx
       ($pref ($@ e ...)))]
    [(_ e ... a:expr)
     (syntax/loc stx
       ($aref ($@ e ...) a))]
    [(_ e ... k:keyword)
     (syntax/loc stx
       ($fref ($@ e ...) (keyword->field 'k)))]))
(provide $@)

;; XXX Generalize this to allow expanding/shrinking ints
(define-class $cast
  #:fields
  [t (Type*-eq-p F64)]
  [e (Expr/c F32)]
  #:methods Expr
  (define (pp)
    (pp:h-append pp:lparen
                 pp:lparen
                 ((Type-pp t) #:name #f #:ptrs 0)
                 pp:rparen
                 ((Expr-pp e))
                 pp:rparen))
  (define (ty) t)
  (define (lval?) #f)
  (define (h! !)
    ((Type-h! t) !)
    ((Expr-h! e) !)))

(define-class $ife
  #:fields
  [c (Expr/c Bool)]
  [t Expr?]
  [f (Expr/c ((Expr-ty t)))]
  #:methods Expr
  (define (pp)
    (pp:h-append
     pp:lparen
     ((Expr-pp c))
     (pp:text " ? ")
     ((Expr-pp t))
     (pp:text " : ")
     ((Expr-pp f))
     pp:rparen))
  (define (ty) ((Expr-ty t)))
  (define (lval?) #f)
  (define (h! !)
    ((Expr-h! c) !)
    ((Expr-h! t) !)
    ((Expr-h! f) !)))

(define-class $seal
  #:fields
  [s symbol?]
  [e Expr?]
  #:methods Expr
  (define (pp) ((Expr-pp e)))
  (define (ty) (Seal s ((Expr-ty e))))
  (define (lval?) #f)
  (define (h! !) ((Expr-h! e) !)))

(define-class $unseal
  #:fields
  [s symbol?]
  [e (Expr?/c (format "sealed with ~a" s)
              (λ (x)
                (and (Seal*? x)
                     (eq? s (Seal*-tag x)))))]
  #:methods Expr
  (define (pp) ((Expr-pp e)))
  (define (ty) (Seal*-st ((Expr-ty e))))
  (define (lval?) #f)
  (define (h! !) ((Expr-h! e) !)))

(define-class Var
  #:fields
  [type Type?]
  [name CName?]
  #:methods Expr
  (define (pp) (pp:text name))
  (define (ty) type)
  (define (lval?) #t)
  (define (h! !) ((Type-h! type) !)))

(define (Var-pp var)
  (λ ()
    ((Type-pp (Var-type var)) #:name (Var-name var) #:ptrs 0)))

(define (pp:op1 o a)
  (pp:h-append pp:lparen (pp:text o) ((Expr-pp a)) pp:rparen))

(define-syntax (define-op1-class stx)
  (syntax-parse stx
    [(_ name:id
        #:ctc arg-ctc:expr
        #:pp arg-pp:expr
        #:ty res-ty:expr)
     (with-syntax ([arg (datum->syntax #'name 'arg)])
       (syntax/loc stx
         (define-class name
           #:fields
           [arg arg-ctc]
           #:methods Expr
           (define (pp) (pp:op1 arg-pp arg))
           (define (ty) res-ty)
           (define (lval?) #f)
           (define (h! !) ((Expr-h! arg) !)))))]))

(define-op1-class $!
  #:ctc (Expr/c Bool)
  #:pp "!"
  #:ty Bool)

(define Int/c (Expr?/c "integer" Int?))

(define Signed-Number/c
  (or/c (Expr?/c "floating" Float?)
        (Expr?/c "signed integer"
                 (λ (x) (and (Int? x) (Int-signed? x))))))

(define Unsigned-Int/c
  (Expr?/c "unsigned integer"
           (λ (x) (and (Int? x) (Int-unsigned? x)))))

(define Number/c
  (Expr?/c "number" Numeric?))

(define-op1-class $neg
  #:ctc Signed-Number/c
  #:pp "-"
  #:ty ((Expr-ty arg)))

(define-class $bneg
  #:fields
  [arg Unsigned-Int/c]
  #:methods Expr
  (define arg-ty ((Expr-ty arg)))
  (define (pp)
    (define arg-ty-pp
      (pp:h-append pp:lparen ((Type-pp arg-ty) #:name #f #:ptrs 0) pp:rparen))
    (pp:h-append arg-ty-pp
                 pp:lparen (pp:text "~")
                 pp:lparen arg-ty-pp ((Expr-pp arg)) pp:rparen
                 pp:rparen))
  (define (ty) arg-ty)
  (define (lval?) #f)
  (define (h! !) ((Expr-h! arg) !)))

(define (pp:op2 a o b)
  (pp:h-append pp:lparen ((Expr-pp a)) pp:space
               (pp:text o) pp:space
               ((Expr-pp b)) pp:rparen))

(define-syntax (define-op2-class stx)
  (syntax-parse stx
    [(_ name:id
        #:lhs-ctc lhs-ctc:expr
        #:pp arg-pp:expr
        (~optional (~seq #:ty res-ty:expr)
                   #:defaults ([res-ty #'((Expr-ty lhs))])))
     (with-syntax ()
       (syntax/loc stx
         (define-class name
           #:fields
           [lhs lhs-ctc]
           [rhs (Expr/c ((Expr-ty lhs)))]
           #:methods Expr
           (define (pp) (pp:op2 lhs arg-pp rhs))
           (define (ty) res-ty)
           (define (lval?) #f)
           (define (h! !)
             ((Expr-h! lhs) !)
             ((Expr-h! rhs) !)))))]))

(define-op2-class $<=
  #:lhs-ctc Number/c
  #:pp "<="
  #:ty Bool)

(define-op2-class $<
  #:lhs-ctc Number/c
  #:pp "<"
  #:ty Bool)

(define-op2-class $>=
  #:lhs-ctc Number/c
  #:pp ">="
  #:ty Bool)

(define-op2-class $>
  #:lhs-ctc Number/c
  #:pp ">"
  #:ty Bool)

(define-op2-class $!=
  #:lhs-ctc
  (or/c Number/c
        (Expr?/c "pointer" *Ptr?))
  #:pp "!="
  #:ty Bool)

(define-op2-class $==
  #:lhs-ctc (or/c Number/c
                  (Expr?/c "pointer" *Ptr?))
  #:pp "=="
  #:ty Bool)

(define-op2-class $*
  #:lhs-ctc Number/c
  #:pp "*")

(define-op2-class $-
  #:lhs-ctc Number/c
  #:pp "-")

(define-op2-class $+
  #:lhs-ctc Number/c
  #:pp "+")

(define-op2-class $/
  #:lhs-ctc Number/c
  #:pp "/")

(define-op2-class $%
  #:lhs-ctc Int/c
  #:pp "%")

(define-op2-class $band
  #:lhs-ctc Unsigned-Int/c
  #:pp "&")
(define-op2-class $bior
  #:lhs-ctc Unsigned-Int/c
  #:pp "|")
(define-op2-class $bxor
  #:lhs-ctc Unsigned-Int/c
  #:pp "^")

(define-op2-class $and
  #:lhs-ctc (Expr/c Bool)
  #:pp "&&")
(define-op2-class $or
  #:lhs-ctc (Expr/c Bool)
  #:pp "||")

(define-syntax (define-bop2-class stx)
  (syntax-parse stx
    [(_ name:id
        #:lhs-ctc lhs-ctc:expr
        #:pp arg-pp:expr
        (~optional (~seq #:ty res-ty:expr)
                   #:defaults ([res-ty #'((Expr-ty lhs))])))
     (with-syntax ()
       (syntax/loc stx
         (define-class name
           #:fields
           [lhs lhs-ctc]
           [rhs (Expr/c ((Expr-ty lhs)))]
           #:methods Expr
           (define (pp)
             (pp:h-append pp:lparen
                          pp:lparen ((Type-pp res-ty) #:name #f #:ptrs 0) pp:rparen
                          (pp:op2 lhs arg-pp rhs)
                          pp:rparen))
           (define (ty) res-ty)
           (define (lval?) #f)
           (define (h! !)
             ((Expr-h! lhs) !)
             ((Expr-h! rhs) !)))))]))

(define-bop2-class $bshl
  #:lhs-ctc Unsigned-Int/c
  #:pp "<<")
(define-bop2-class $bshr
  #:lhs-ctc Unsigned-Int/c
  #:pp ">>")

(define-class $val
  #:fields
  [vty Type?]
  [val (Type-val? vty)]
  #:methods Expr
  (define (pp) (pp:h-append pp:lparen
                            ;; pp:lparen ((Type-pp vty) #:name #f #:ptrs 0) pp:rparen
                            ((Type-pp:val vty) val)
                            pp:rparen))
  (define (ty) vty)
  (define (lval?) #f)
  (define (h! !)
    ((Type-h! vty) !)))

(define-class-alias $NULL ($val (Ptr Any) the-NULL))

(define (Fun-args/c f)
  (apply list/c (map Expr/c (Fun-dom f))))

(define-class $%app
  #:fields
  [rator (or/c (Expr?/c "function" Fun?)
               (Expr?/c "function ptr"
                        (λ (x)
                          (and (*Ptr? x)
                               (Fun? (*Ptr-st x))))))]
  [rands
   (let ([rator-ty ((Expr-ty rator))])
     (cond
       [(Any? rator-ty)
        (listof Expr?)]
       [(Fun? rator-ty)
        (Fun-args/c rator-ty)]
       [(*Ptr? rator-ty)
        (Fun-args/c (*Ptr-st rator-ty))]))]
  #:methods Expr
  (define (pp)
    (pp:h-append ((Expr-pp rator)) pp:lparen
                 (apply pp:hs-append
                        (pp:apply-infix pp:comma
                                        (map (λ (r) ((Expr-pp r))) rands)))
                 pp:rparen))
  (define (ty)
    (define rator-ty ((Expr-ty rator)))
    (cond
      [(Any? rator-ty)
       Any]
      [(Fun? rator-ty)
       (Fun-rng rator-ty)]
      [(*Ptr? rator-ty)
       (Fun-rng (*Ptr-st rator-ty))]))
  (define (lval?) #f)
  (define (h! !)
    ((Expr-h! rator) !)
    (for ([r (in-list rands)])
      ((Expr-h! r) !))))

;;; Statement

(define-interface Stmt
  [pp
   (-> pp:doc?)]
  [h!
   (-> (-> CHeader? void?)
       void?)]
  [ret?
   (-> boolean?)])

(define NEST 2)

(define-class $nop
  #:fields
  #:methods Stmt
  (define (pp) (pp:text ";"))
  (define (h! !) (void))
  (define (ret?) #f))

(define-class $seq
  #:fields
  [a Stmt?] [b Stmt?]
  #:methods Stmt
  (define (pp)
    (pp:v-append ((Stmt-pp a)) ((Stmt-pp b))))
  (define (h! !)
    ((Stmt-h! a) !)
    ((Stmt-h! b) !))
  (define (ret?)
    ((Stmt-ret? b))))

(define-class $do
  #:fields
  [e (Expr/c Void)]
  #:methods Stmt
  (define (pp)
    (pp:h-append (pp:text "(void)") ((Expr-pp e)) pp:semi))
  (define (h! !)
    ((Expr-h! e) !))
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
  (define (h! !)
    ((Expr-h! test) !)
    ((Stmt-h! ift) !)
    ((Stmt-h! iff) !))
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
  (define (h! !)
    ((Expr-h! test) !)
    ((Stmt-h! body) !))
  (define (ret?)
    ((Stmt-ret? body))))

(define-class $%let1
  #:fields
  [hn (or/c false/c symbol?)]
  [vty Type?]
  [bodyf (-> Var? Stmt?)]
  #:methods Stmt
  (define var (Var vty (gencsym hn)))
  (define body (bodyf var))
  (define (pp)
    (define the-init ((Type-pp:init vty)))
    (pp:h-append pp:lbrace pp:space ((Var-pp var))
                 (if the-init
                     (pp:h-append pp:space (pp:char #\=) pp:space
                                  the-init)
                     pp:empty)
                 pp:semi
                 (pp:nest NEST (pp:h-append pp:line ((Stmt-pp body)))) pp:rbrace))
  (define (h! !)
    ((Stmt-h! body) !))
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
  (define (h! !)
    ((Expr-h! lval) !)
    ((Expr-h! rhs) !))
  (define (ret?)
    #f))

(define-class $ret/ty
  #:fields
  [ty Type?]
  [e (or/c false/c (Expr/c ty))]
  #:methods Stmt
  (define (pp)
    (pp:h-append (pp:text "return")
                 (if (Void? ty)
                     pp:empty
                     (pp:h-append pp:space ((Expr-pp e))))
                 pp:semi))
  (define (h! !)
    (when e ((Expr-h! e) !)))
  (define (ret?) #t))

;; Declaration

(define-interface Decl
  [ty
   (-> Type?)]
  [name
   (-> CName?)]
  [visit!
   (-> #:headers! (-> CHeader? void?)
       (->
        #:proto-only? boolean?
        pp:doc?))])

(define-syntax (define-$dref-Expr stx)
  (syntax-parse stx
    [(_)
     (with-syntax ([(pp ty lval? h!)
                    (for/list ([i (in-list '(pp ty lval? h!))])
                      (datum->syntax stx i))])
       (syntax/loc stx
         (begin (define (pp) (pp:text ((Decl-name this))))
                (define (ty) ((Decl-ty this)))
                (define (lval?) #t)
                (define (h! !) (ec-add-decl! this)))))]))

(define-class $extern
  #:fields
  [h CHeader?]
  [n CName?]
  [ety Type?]
  #:procedure $%app
  #:methods Expr (define-$dref-Expr)
  #:methods Decl
  (define (ty) ety)
  (define (name) n)
  (define (visit! #:headers! !)
    (! h)
    ((Type-h! ety) !)
    (λ (#:proto-only? po?)
      pp:empty)))

(define-class $%proc
  #:fields
  [hn symbol?]
  [pty Fun?]
  [hns (listof (or/c false/c symbol?))]
  [body (dynamic->*
         #:mandatory-domain-contracts (make-list (length (Fun-dom pty)) Var?)
         #:range-contracts (list
                            (and/c Stmt?
                                   (flat-named-contract
                                    "returns"
                                    (λ (x)
                                      ((Stmt-ret? x)))))))]
  #:procedure $%app
  #:methods Expr (define-$dref-Expr)
  #:methods Decl
  (define (ty) pty)

  (define default-name (gencsym hn))
  (define (name)
    (or (ec-global-name this)
        default-name))

  (define (visit! #:headers! headers!)
    (define global? (ec-global-name this))
    (define n (name))
    (define maybe-static
      (if global? pp:empty (pp:h-append (pp:text "static") pp:space)))
    (define dom
      (Fun-dom pty))
    (define vs
      (for/list ([t (in-list dom)]
                 [hn (in-list hns)])
        (Var t (gencsym hn))))
    ((Type-h! pty) headers!)
    (define the-body (apply body vs))
    ((Stmt-h! the-body) headers!)
    (define body-pp ((Stmt-pp the-body)))
    (λ (#:proto-only? proto-only?)
      (pp:h-append
       maybe-static
       (pp:h-append
        (pp:hs-append ((Type-pp (Fun-rng pty)) #:name #f #:ptrs 0) (pp:text n)
                      pp:lparen
                      (apply pp:hs-append
                             (pp:apply-infix
                              pp:comma
                              (if proto-only?
                                  (for/list ([t (in-list dom)])
                                    ((Type-pp t) #:name #f #:ptrs 0))
                                  (for/list ([v (in-list vs)])
                                    ((Var-pp v))))))
                      pp:rparen)
        (if proto-only?
            pp:semi
            (pp:h-append
             pp:space
             (pp:nest NEST
                      (pp:h-append
                       pp:lbrace pp:line
                       body-pp))
             pp:line
             pp:rbrace)))))))

;; Unit
(require racket/set)

(struct emit-result (cflags ldflags doc))

(define-interface Unit
  [emit (-> emit-result?)])

(define-class $cflags
  #:fields
  [fls (listof string?)]
  [u Unit?]
  #:methods Unit
  (define (emit)
    (define er ((Unit-emit u)))
    (struct-copy emit-result er
                 [cflags (append fls (emit-result-cflags er))])))

(define-class $ldflags
  #:fields
  [fls (listof string?)]
  [u Unit?]
  #:methods Unit
  (define (emit)
    (define er ((Unit-emit u)))
    (struct-copy emit-result er
                 [ldflags (append fls (emit-result-ldflags er))])))

(define <stdint.h>
  (CHeader '() '() '() "<stdint.h>" '()))
(define <stddef.h>
  (CHeader '() '() '() "<stddef.h>" '()))

(define-syntax-rule (while t . b)
  (let loop ()
    (when t
      (begin . b)
      (loop))))

(struct emit-context
  (headers
   header-pps
   decls
   decl->gname
   decl->proto-pp
   decl->pp
   struct->forward-pp
   struct->def-pp))
(define (make-emit-context)
  (emit-context (mutable-seteq)
                (mutable-seteq)
                (mutable-seteq)
                (make-hasheq)
                (make-hasheq)
                (make-hasheq)
                (make-hasheq)
                (make-hasheq)))
(define current-ec (make-parameter #f))
(define (current-ec*)
  (or (current-ec)
      (error 'Decl "Cannot reference declaration outside of emit")))
(define (ec-global-name d)
  (hash-ref (emit-context-decl->gname (current-ec*)) d #f))
(define (ec-global-name! d n)
  (hash-set! (emit-context-decl->gname (current-ec*)) d n))
(define (ec-add-decl! d)
  (set-add! (emit-context-decls (current-ec*)) d))
(define (ec-struct! s f d)
  (define ec (current-ec*))
  (define fpp (emit-context-struct->forward-pp ec))
  (cond
    [(hash-has-key? fpp s)
     #f]
    [else
     (hash-set! fpp s f)
     (hash-set! (emit-context-struct->def-pp ec) s d)
     #t]))
(define (ec-fixed-point! ec)
  (define hs (emit-context-headers ec))
  (define h-pp (emit-context-header-pps ec))
  (define (h! h)
    (unless (set-member? hs h)
      (set-add! hs h)
      (set-add! h-pp (pp:header h))))
  (h! <stdint.h>)
  (h! <stddef.h>)
  (define d->proto-pp (emit-context-decl->proto-pp ec))
  (define d->pp (emit-context-decl->pp ec))
  (let repeat ()
    (and
     (for/or ([d (in-set (emit-context-decls ec))]
              #:unless (hash-has-key? d->pp d))
       (define ppf
         ((Decl-visit! d) #:headers! h!))
       (hash-set! d->proto-pp d (ppf #:proto-only? #t))
       (hash-set! d->pp d (ppf #:proto-only? #f)))
     (repeat))))

(define-class $lib
  #:fields
  [lib (hash/c CName? Decl?)]
  #:methods Unit
  (define (emit)
    (define ec (make-emit-context))
    (parameterize ([current-ec ec])
      (for ([(k v) (in-hash lib)])
        (ec-global-name! v k)
        (ec-add-decl! v))
      (ec-fixed-point! ec))

    (define (h-collect f)
      (append*
       (for/list ([i (in-set (emit-context-headers ec))])
         (f i))))

    (define (v-append-l l)
      (pp:v-append
       (apply pp:v-append l)
       pp:line))
    (define (v-append-hash ht)
      (v-append-l (hash-values ht)))
    (define (v-append-set s)
      (v-append-l (set->list s)))

    (emit-result
     (h-collect CHeader-cflags)
     (h-collect CHeader-ldflags)
     (pp:v-append
      (v-append-set (emit-context-header-pps ec))
      (v-append-hash (emit-context-struct->forward-pp ec))
      (v-append-hash (emit-context-struct->def-pp ec))
      (v-append-hash (emit-context-decl->proto-pp ec))
      (v-append-hash (emit-context-decl->pp ec))))))

(define-simple-macro ($exe p)
  ($lib (hash "main" p)))
(provide $exe)

;; Compiler

(define (emit u)
  ((Unit-emit u)))

(define (er-print! er)
  (pp:pretty-print (emit-result-doc er)))

(define (emit! u)
  (er-print! (emit u)))
(provide
 (contract-out
  [emit! (-> Unit? void?)]))

(require racket/file
         racket/system)
(define (delete-file* p)
  (when (file-exists? p)
    (delete-file p)))

(define (call-with-temporary pt t)
  (define p (make-temporary-file pt))
  (dynamic-wind
    void
    (λ () (t p))
    (λ ()
      (delete-file* p))))
(provide call-with-temporary)

(define (output-c! c-p er)
  (with-output-to-file c-p
    #:exists 'replace
    (λ () (er-print! er))))

(define (compile! c-p o-p er)
  (or (apply system* (find-executable-path "cc")
             (append (emit-result-cflags er)
                     (list c-p "-c" "-o" o-p)))
      (error 'run "Failed to compile"))
  (void))

(define (link-bin! object-p bin-p er #:more-flags [more-flags '()])
  (or (apply system* (find-executable-path "cc")
             (append (list object-p) (emit-result-ldflags er)
                     more-flags (list "-o" bin-p)))
      (error 'link "Failed to link"))
  (void))

(define (link-lib! object-p lib-p er)
  (link-bin! object-p lib-p er
             #:more-flags
             (match (system-type 'os)
               ['macosx '("-dynamiclib")])))

(define (compile&f u f)
  (define er (emit u))
  (call-with-temporary
   "~a.c"
   (λ (c)
     (output-c! c er)
     (call-with-temporary
      "~a.o"
      (λ (o)
        (compile! c o er)
        (call-with-temporary
         "~a.bin"
         (λ (b)
           (link-bin! o b er)
           (f b))))))))

(define (run u)
  (compile&f u (λ (b) (system* b) (void))))

(define (run&capture u)
  (compile&f u
             (λ (b)
               (define bs-stdout (open-output-string))
               (define-values (sp stdout stdin stderr)
                 (subprocess #f (current-input-port) (current-error-port) b))
               (local-require racket/port)
               (define cpt (thread (λ () (copy-port stdout bs-stdout))))
               (subprocess-wait sp)
               (thread-wait cpt)
               (close-input-port stdout)
               (get-output-string bs-stdout))))

(provide
 (contract-out
  [emit (-> Unit? emit-result?)]
  [output-c! (-> path-string? emit-result? void?)]
  [compile! (-> path-string? path-string? emit-result? void?)]
  [link-lib! (-> path-string? path-string? emit-result? void?)]
  [compile&f (-> Unit? (-> path-string? any) any)]
  [run (-> Unit? void?)]
  [run&capture (-> Unit? string?)]))

;; Convenience
(require racket/stxparam)

(define-syntax-parameter $ret
  (λ (stx)
    (raise-syntax-error '$ret "Illegal outside $proc")))
(define-syntax-parameter $return
  (λ (stx)
    (raise-syntax-error '$return "Illegal outside $proc")))
(provide $ret $return)

(define-syntax ($proc stx)
  (syntax-parse stx
    [(_ ([vt:expr v:id] ...) rt:expr
        . body)
     (quasisyntax/loc stx
       ($%proc (or '#,(syntax-local-name) (gensym '$proc))
               (Fun (list vt ...) rt)
               '(v ...)
               (λ (v ...)
                 (define-class-alias this-$ret ($ret/ty rt) #:no-provide)
                 (define-class-alias this-$return ($ret/ty rt #f) #:no-provide)
                 (syntax-parameterize ([$ret (make-rename-transformer #'this-$ret)]
                                       [$return (make-rename-transformer #'this-$return)])
                   . body))))]))
(provide $proc)

(define-syntax ($app stx)
  (syntax-parse stx
    [(_ rator rand ...)
     (syntax/loc stx
       ($%app rator (list rand ...)))]))
(provide $app)

(define ($v* v)
  (match v
    [(? string?)
     ($v String v)]
    [(? boolean?)
     ($v Bool v)]
    [_
     (error '$v "cannot infer type of ~e" v)]))

(define-syntax ($v stx)
  (syntax-parse stx
    [(_ v:expr)
     (syntax/loc stx
       ($v* v))]
    [(_ t:expr v:expr)
     (syntax/loc stx
       ($val t v))]))
(provide $v)

(define-syntax ($begin stx)
  (syntax-parse stx
    [(_) (syntax/loc stx ($nop))]
    [(_ s) #'s]
    [(_ s . ss)
     (quasisyntax/loc #'s
       ($seq s ($begin . ss)))]))
(provide $begin)

(define-syntax ($let1 stx)
  (syntax-parse stx
    [(_ ([ty:expr n:id e:expr]) . b)
     (quasisyntax/loc stx
       ($%let1 'n ty
               #,(quasisyntax/loc stx
                   (λ (n)
                     ($seq ($set! n e)
                           #,(quasisyntax/loc #'b
                               ($begin . b)))))))]
    [(_ ([ty:expr n:id]) . b)
     (quasisyntax/loc stx
       ($%let1 'n ty
               #,(quasisyntax/loc stx
                   (λ (n)
                     #,(quasisyntax/loc #'b
                         ($begin . b))))))]))
(provide $let1)

(define-syntax ($let* stx)
  (syntax-parse stx
    [(_ () . b)
     (syntax/loc stx
       ($begin . b))]
    [(_ (f . more) . b)
     (syntax/loc stx
       ($let1 (f)
              ($let* more . b)))]))
(provide $let*)

(define-syntax ($while stx)
  (syntax-parse stx
    [(_ e . b)
     (quasisyntax/loc stx
       ($%while e
                #,(syntax/loc #'b ($begin . b))))]))
(define-syntax ($when stx)
  (syntax-parse stx
    [(_ e . b)
     (quasisyntax/loc stx
       ($if e #,(syntax/loc #'b ($begin . b)) ($nop)))]))
(define-syntax ($unless stx)
  (syntax-parse stx
    [(_ e . b)
     (quasisyntax/loc stx
       ($if #,(syntax/loc stx ($! e))
            #,(syntax/loc #'b ($begin . b))
            ($nop)))]))
(provide $while $when $unless)

(struct iter (init test step))
(define-simple-macro ($for ([ty:expr i:id iter-e:expr]) . b)
  (let ([iteri iter-e])
    ($let1 ([ty i ((iter-init iteri) ty)])
           ($%while ((iter-test iteri) ty i)
                    ($seq ($begin . b) ((iter-step iteri) ty i))))))
(provide $for)

(define ($in-range e)
  (iter (λ (ty) ($val ty 0))
        (λ (ty i) ($< i e))
        (λ (ty i) ($set! i ($+ i ($val ty 1))))))
(provide $in-range)

(define ($default-flags u)
  ($cflags '("-Wall" "-Wextra" "-Weverything" "-Wpedantic" "-Wshadow"
             "-Wstrict-overflow" "-fno-strict-aliasing"
             "-Wno-parentheses-equality"
             #;"-Wno-unused-parameter"
             "-Wno-unused-function"
             "-Wno-tautological-compare"
             "-Werror" "-pedantic" "-std=c99" "-O3" "-march=native"
             "-fno-stack-protector" "-ffunction-sections" "-fdata-sections"
             "-fno-unwind-tables" "-fno-asynchronous-unwind-tables" "-fno-math-errno"
             "-fmerge-all-constants" "-fno-ident" "-fPIE" "-fPIC")
           ($ldflags '("-dead_strip") u)))
(provide $default-flags)