#lang racket/base
;; Library
(require (for-syntax racket/base
                     racket/syntax
                     syntax/parse
                     syntax/srcloc)
         syntax/location
         racket/contract/base
         racket/contract/combinator
         racket/generic
         racket/match
         racket/list
         syntax/parse/define)

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

(define-syntax (define-class stx)
  (syntax-parse stx
    [(_ name:id
        #:fields [f:id f-ctc:expr] ...
        (~optional (~seq #:procedure proc:id))
        (~seq #:methods iname
              idef:expr ...))
     #:declare iname (static interface-info? "interface")
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
           (define (make-name-object fields)
             (match-define (_name-fields srcloc f ...) fields)
             idef ...
             (name-object
              (make-immutable-hasheq
               (list
                (cons
                 _inst:name
                 (make-immutable-hasheq
                  (list (cons _inst:name-m m^)
                        ...)))))
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

;;; General
(require (prefix-in pp: pprint))

;; XXX make sure string is okay
(define CName? string?)

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
(define-srcloc-struct $NULL)

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
   (-> any/c ;; XXX really should be val?
       pp:doc?)])

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
    (error 'Any "Values cannot be printed")))

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
  [lit string?]
  [v? (-> any/c boolean?)]
  ;; XXX should be v?
  [ppv (-> any/c pp:doc?)]
  #:methods Type
  (define (name) lit)
  (define (pp #:name n #:ptrs p)
    (pp:ty-name lit n p))
  (define (h! !) (void))
  (define (eq t)
    (and (Literal? t)
         (equal? lit (Literal-lit t))))
  (define (val? x) (v? x))
  (define (pp:val v) (ppv v)))

(define (pp:char-val c)
  (pp:text (number->string (char->integer c))))
(define (never? x) #f)

(define-class-alias Size (Literal "size_t" never? pp:never))
(define-class-alias Char (Literal "char" char? pp:char-val))
(define-class-alias Void (Literal "void" never? pp:never))
(define Void? (Type-eq Void))

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
    (pp:text (number->string v))))

(define (Int-unsigned? t)
  (not (Int-signed? t)))

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
    ;; XXX what if number->string doesn't include "."
    (pp:h-append (pp:text (number->string v))
                 (match bits
                   [32 (pp:text "f")]
                   [64 pp:empty]))))

(define-class-alias F32 (Float 32))
(define-class-alias F64 (Float 64))

(define (Numeric? x) (or (Int? x) (Float? x)))

(define-class Ptr
  #:fields
  [st Type?]
  #:methods Type
  (define (name)
    (format "ptr(~a)" ((Type-name st))))
  (define (pp #:name n #:ptrs p)
    ((Type-pp st) #:name n #:ptrs (add1 p)))
  (define (h! !) ((Type-h! st) !))
  (define (eq t)
    (and (Ptr? t)
         ((Type-eq st) t)))
  (define (val? x) ($NULL? x))
  (define (pp:val v)
    (pp:text "NULL")))

;; XXX Record Arr Union

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
                  ((Type-eq t) s))))
         ((Type-eq (Fun-rng t)) rng)))
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
  (define pp:val pp:never))

;; XXX Opaque Extern

(define-class Seal*
  #:fields
  [v? (or/c #f (-> any/c boolean?))]
  ;; XXX should be (if v? v? (Type-val? st))
  [ppv (-> any/c pp:doc?)]
  [tag symbol?] [st Type?]  
  #:methods Type
  (define (name) (format "~a(~a)" tag ((Type-name st))))
  (define (pp #:name n #:ptrs p)
    ((Type-pp st) #:name n #:ptrs p))
  (define (h! !)
    ((Type-h! st) !))
  (define (eq t)
    (and (Seal*? t)
         (eq? tag (Seal*-tag t))
         ((Type-eq st) (Seal*-st t))))
  (define (val? x)
    (if v?
        (v? x)
        ((Type-val? st) x)))
  (define (pp:val v)
    (if ppv
        (ppv v)
        ((Type-pp:val st) v))))

(define-class-alias Seal (Seal* #f #f))

(define (pp:bool v)
  (pp:text (number->string (if v 1 0))))
(define-class-alias Bool (Seal* boolean? pp:bool 'Bool UI8))

(define (pp:cstring v)
  (pp:text (format "~v" v)))
(define-class-alias String (Seal* string? pp:cstring 'String (Ptr Char)))

(define (gencsym [s 'c])
  (symbol->string (gensym (regexp-replace* #rx"[^A-Za-z_0-9]" (format "_~a" s) "_"))))

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
   (Type-eq ty)))

(define (Expr?/c lab ty?)
  (define name (format "Expr with ~a" lab))
  (make-flat-contract
   #:name name
   #:first-order Expr?
   #:projection
   (λ (b)
     (λ (x)
       (if (Expr? x)
           (let ([xt ((Expr-ty x))])
             (if (or (Any? xt) (ty? xt))
                 x
                 (raise-blame-error
                  b x
                  '(expected: "~a" given: "Expr(~e) with type ~e")
                  name x xt)))
           (raise-blame-error
            b x
            '(expected: "~a" given: "~e")
            name x))))))

(define Lval/c
  (and/c Expr?
         (flat-named-contract "Expr-lval?"                              
                              (λ (x)
                                ((Expr-lval? x))))))

;; XXX Exprs: $sizeof $offsetof $aref $addr $pref $sref $uref $ife
;; $seal $unseal

(define-class $dref
  #:fields
  [d Decl?]
  #:methods Expr
  (define ec (current-ec))
  (ec-add-decl! ec d #f)
  (define (pp) (pp:text (hash-ref (emit-context-decl->name ec) d)))
  (define (ty) (hash-ref (emit-context-decl->ty ec) d))
  (define (lval?) #f)
  (define (h! !) (void)))

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

;; XXX These seem macro-producible, but I get an erro about pp not
;; being bound.

(define-class $!
  #:fields
  [arg (Expr/c Bool)]
  #:methods Expr
  (define (pp) (pp:op1 "!" arg))
  (define (ty) Bool)
  (define (lval?) #f)
  (define (h! !) ((Expr-h! arg) !)))

(define-class $neg
  #:fields
  [arg (or/c (Expr?/c "floating" Float?)
             (Expr?/c "signed integer"
                      (λ (x) (and (Int? x) (Int-signed? x)))))]
  #:methods Expr
  (define (pp) (pp:op1 "-" arg))
  (define (ty) ((Expr-ty arg)))
  (define (lval?) #f)
  (define (h! !) ((Expr-h! arg) !)))

(define-class $bneg
  #:fields
  [arg (Expr?/c "unsigned integer"
                (λ (x) (and (Int? x) (Int-unsigned? x))))]
  #:methods Expr
  (define (pp) (pp:op1 "~" arg))
  (define (ty) ((Expr-ty arg)))
  (define (lval?) #f)
  (define (h! !) ((Expr-h! arg) !)))

;; XXX Op2: $/ $% $== $> $>= $and $or $band $bior
;; $bxor $bshl $bshr

(define (pp:op2 a o b)
  (pp:h-append pp:lparen ((Expr-pp a)) pp:space
               (pp:text o) pp:space
               ((Expr-pp b)) pp:rparen))

;; XXX These should be macro applications

(define-class $<=
  #:fields
  [lhs (Expr?/c "integer" Int?)]
  [rhs (Expr/c ((Expr-ty lhs)))]
  #:methods Expr
  (define (pp) (pp:op2 lhs "<=" rhs))
  (define (ty) Bool)
  (define (lval?) #f)
  (define (h! !)
    ((Expr-h! lhs) !)
    ((Expr-h! rhs) !)))

(define-class $<
  #:fields
  [lhs (Expr?/c "integer" Int?)]
  [rhs (Expr/c ((Expr-ty lhs)))]
  #:methods Expr
  (define (pp) (pp:op2 lhs "<" rhs))
  (define (ty) Bool)
  (define (lval?) #f)
  (define (h! !)
    ((Expr-h! lhs) !)
    ((Expr-h! rhs) !)))

(define-class $!=
  #:fields
  [lhs (or/c (Expr?/c "number" Numeric?)
             (Expr?/c "pointer" Ptr?))]
  [rhs (Expr/c ((Expr-ty lhs)))]
  #:methods Expr
  (define (pp) (pp:op2 lhs "!=" rhs))
  (define (ty) Bool)
  (define (lval?) #f)
  (define (h! !)
    ((Expr-h! lhs) !)
    ((Expr-h! rhs) !)))

(define-class $*
  #:fields
  [lhs (Expr?/c "number" Numeric?)]
  [rhs (Expr/c ((Expr-ty lhs)))]
  #:methods Expr
  (define (pp) (pp:op2 lhs "*" rhs))
  (define (ty) ((Expr-ty lhs)))
  (define (lval?) #f)
  (define (h! !)
    ((Expr-h! lhs) !)
    ((Expr-h! rhs) !)))

(define-class $-
  #:fields
  [lhs (Expr?/c "number" Numeric?)]
  [rhs (Expr/c ((Expr-ty lhs)))]
  #:methods Expr
  (define (pp) (pp:op2 lhs "-" rhs))
  (define (ty) ((Expr-ty lhs)))
  (define (lval?) #f)
  (define (h! !)
    ((Expr-h! lhs) !)
    ((Expr-h! rhs) !)))

(define-class $+
  #:fields
  [lhs (Expr?/c "number" Numeric?)]
  [rhs (Expr/c ((Expr-ty lhs)))]
  #:methods Expr
  (define (pp) (pp:op2 lhs "+" rhs))
  (define (ty) ((Expr-ty lhs)))
  (define (lval?) #f)
  (define (h! !)
    ((Expr-h! lhs) !)
    ((Expr-h! rhs) !)))

(define-class $val
  #:fields
  [vty Type?]
  [val (Type-val? vty)]
  #:methods Expr
  (define (pp) ((Type-pp:val vty) val))
  (define (ty) vty)
  (define (lval?) #f)
  (define (h! !)
    ((Type-h! vty) !)))

(define-class $%app
  #:fields
  [rator (Expr?/c "function" Fun?)]
  [rands
   (let ([rator-ty ((Expr-ty rator))])
     (if (Any? rator-ty)
         (listof Expr?)
         (apply list/c (map Expr/c (Fun-dom rator-ty)))))]
  #:methods Expr
  (define (pp)
    (pp:h-append ((Expr-pp rator)) pp:lparen
                 (apply pp:hs-append
                        (pp:apply-infix pp:comma
                                        (map (λ (r) ((Expr-pp r))) rands)))
                 pp:rparen))
  (define (ty)
    (define rator-ty ((Expr-ty rator)))
    (if (Any? rator-ty)
        Any
        (Fun-rng rator-ty)))
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
    (pp:h-append pp:lbrace pp:space ((Var-pp var)) pp:semi
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
  [e (Expr/c ty)]
  #:methods Stmt
  (define (pp)
    (pp:h-append (pp:text "return")
                 (if (Void? ty)
                     pp:empty
                     (pp:h-append pp:space ((Expr-pp e))))
                 pp:semi))
  (define (h! !)
    ((Expr-h! e) !))
  (define (ret?) #t))

;; Declaration

(define-interface Decl
  [ty
   (-> Type?)]
  [name
   (-> CName?)]
  [hint
   (-> (or/c false/c symbol?))]
  [visit!
   (-> #:headers! (-> CHeader? void?)
       #:global? boolean?
       #:name CName?
       (->
        #:proto-only? boolean?
        pp:doc?))])

(define ($dref-$%app d args)
  ($%app ($dref d) args))

(define-class $extern
  #:fields
  [h CHeader?]
  [n CName?]
  [ety Type?]
  #:procedure $dref-$%app
  #:methods Decl
  (define (hint) #f)
  (define (ty) ety)
  (define (name) n)
  (define (visit! #:headers! ! #:global? global? #:name n)
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
         #:range-contracts (list Stmt?))]
  #:procedure $dref-$%app
  #:methods Decl
  (define (hint) hn)
  (define (ty) pty)
  (define (name) (gencsym hn))
  (define (visit! #:headers! headers! #:global? global? #:name n)
    (define maybe-static
      (if global? pp:empty (pp:h-append (pp:text "static") pp:space)))
    (define dom
      (Fun-dom pty))
    (define vs
      (for/list ([t (in-list dom)]
                 [hn (in-list hns)])
        (Var t (gencsym hn))))
    ((Type-h! pty) headers!)
    (define the-body
      (apply body vs))
    ((Stmt-h! the-body) headers!)
    (define body-pp
      ((Stmt-pp the-body)))
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

(struct emit-context
  (headers
   decls
   decl->name
   decl->ty
   decl->proto-pp
   decl->pp))
(define (make-emit-context)
  (emit-context (mutable-seteq <stdint.h>)
                (mutable-seteq)
                (make-hasheq)
                (make-hasheq)
                (make-hasheq)
                (make-hasheq)))
(define current-ec (make-parameter #f))
(define (ec-add-decl! ec d n)
  (define ds (emit-context-decls ec))
  (unless (set-member? ds d)
    (set-add! ds d)
    (hash-set! (emit-context-decl->name ec) d (or n ((Decl-name d))))
    (define ty ((Decl-ty d)))
    (hash-set! (emit-context-decl->ty ec) d ty)))
(define (ec-fixed-point! ec)
  (define repeat? #f)
  (define hs (emit-context-headers ec))
  (define d->n (emit-context-decl->name ec))
  (define d->proto-pp (emit-context-decl->proto-pp ec))
  (define d->pp (emit-context-decl->pp ec))
  (for ([d (in-set (emit-context-decls ec))]
        #:unless (hash-has-key? d->pp d))
    (set! repeat? #t)
    (define n (hash-ref! d->n d (λ () ((Decl-name d)))))
    (define ppf
      ((Decl-visit! d)
       #:headers!
       (λ (h)
         (set-add! hs h))
       #:global? (string=? n "main")
       #:name n))
    (hash-set! d->proto-pp d (ppf #:proto-only? #t))
    (hash-set! d->pp d (ppf #:proto-only? #f)))
  (when repeat?
    (ec-fixed-point! ec)))
(define (pp:decl-proto ec d)
  (hash-ref (emit-context-decl->proto-pp ec) d))
(define (pp:decl ec d)
  (hash-ref (emit-context-decl->pp ec) d))

(define-class $exe
  #:fields
  [main $%proc?]
  #:methods Unit
  (define (emit)
    (define ec (make-emit-context))
    (parameterize ([current-ec ec])
      (ec-add-decl! ec main "main")
      (ec-fixed-point! ec))
    (define cflags
      (append*
       (for/list ([i (in-set (emit-context-headers ec))])
         (CHeader-cflags i))))
    (define ldflags
      (append*
       (for/list ([i (in-set (emit-context-headers ec))])
         (CHeader-ldflags i))))
    (emit-result
     cflags
     ldflags
     (pp:v-append (apply pp:v-append
                         (for/list ([i (in-set (emit-context-headers ec))])
                           (pp:header i)))
                  pp:line
                  (apply pp:v-append
                         (for/list ([i (in-set (emit-context-decls ec))])
                           (pp:decl-proto ec i)))
                  pp:line
                  (apply pp:v-append
                         (for/list ([i (in-set (emit-context-decls ec))])
                           (pp:h-append (pp:decl ec i) pp:line)))))))

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

(define (run u)
  (define er (emit u))
  (call-with-temporary
   "~a.c"
   (λ (c)
     (with-output-to-file c #:exists 'replace (λ () (er-print! er)))
     (call-with-temporary
      "~a.o"
      (λ (o)
        (define cc-pth (find-executable-path "cc"))
        (or (apply system* cc-pth (append (emit-result-cflags er) (list c "-c" "-o" o)))
            (error 'run "Failed to compile"))
        (call-with-temporary
         "~a.bin"
         (λ (b)
           (or (apply system* cc-pth
                      (append (list o) (emit-result-ldflags er) (list "-o" b)))
               (error 'run "Failed to link"))
           (system* b)
           (void))))))))
(provide
 (contract-out
  [run (-> Unit? void?)]))

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
(provide $*)

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
                               ($begin . b)))))))]))
(provide $let1)

(define-simple-macro ($while e . b)
  ($%while e ($begin . b)))
(define-simple-macro ($when e . b)
  ($if e ($begin . b) ($nop)))
(define-simple-macro ($unless e . b)
  ($when ($! e) . b))
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
             "-Wno-unused-parameter" "-Wno-unused-function"
             "-Werror" "-pedantic" "-std=c99" "-O3" "-march=native"
             "-fno-stack-protector" "-ffunction-sections" "-fdata-sections"
             "-fno-unwind-tables" "-fno-asynchronous-unwind-tables" "-fno-math-errno"
             "-fmerge-all-constants" "-fno-ident" "-fPIE" "-fPIC")
           ($ldflags '("-dead_strip") u)))
(provide $default-flags)

(define <stdio.h> (CHeader '() '() '() "<stdio.h>" '()))
(define $printf ($extern <stdio.h> "printf" Any))
(provide <stdio.h>
         $printf)

(module* test racket/base
  (require (submod ".."))
  (define fac-rec
    ($proc ([UI64 n]) UI64
           ($if ($<= n ($v UI64 0))
                ($ret ($v UI64 1))
                ($ret ($* n
                          (fac-rec ($- n ($v UI64 1))))))))
  (define fac
    ($proc ([UI64 n]) UI64
           ($let1 ([UI64 acc ($v UI64 1)])
                  ($while ($!= n ($v UI64 0))
                          ($set! acc ($* acc n))
                          ($set! n ($- n ($v UI64 1))))
                  ($ret acc))))
  (define main
    ($proc () SI32
           (define (test-fac which fac)
             ($let1 ([UI64 r ($v UI64 0)])
                    ($for ([UI32 i ($in-range ($v UI32 10000))])
                          ($set! r (fac ($v UI64 12))))
                    ($do ($printf ($v (format "~a r = %llu\n" which)) r))))
           ($begin (test-fac "iter" fac)
                   (test-fac " rec" fac-rec)
                   ($ret ($v SI32 0)))))
  (define this
    ($default-flags ($exe main)))
  (emit! this)
  (run this))
