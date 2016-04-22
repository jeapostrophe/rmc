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
         syntax/parse/define
         "class.rkt")

;;; General
(require (prefix-in pp: pprint))

(define (CName? x)
  (and (string? x)
       (regexp-match #rx"^[_a-zA-Z][_a-zA-Z0-9]*$" x)))

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
   (-> any/c
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
       ;; XXX basically nothing is a single flonum
       [32 single-flonum?]
       [64 double-flonum?])
     x))
  (define (pp:val v)
    (pp:h-append (pp:text
                  ;; XXX It would be nice if this would never fail.
                  (contract
                   (λ (x)
                     (regexp-match #rx"^[-+]?[0-9]+\\.[0-9]+$"
                                   x))
                   (number->string v)
                   'racket 'racket))
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
         ((Type-eq st) (Ptr-st t))))
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

;; XXX Exprs: $sizeof $offsetof $aref $addr $pref $sref $uref

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

(define-class $dref
  #:fields
  [d Decl?]
  #:methods Expr
  ;; XXX Put this somewhere else?
  (define ec (or (current-ec)
                 (error '$dref "Cannot reference declaration outside of emit")))
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
  #:lhs-ctc (or/c Number/c
                  (Expr?/c "pointer" Ptr?))
  #:pp "!="
  #:ty Bool)

(define-op2-class $==
  #:lhs-ctc (or/c Number/c
                  (Expr?/c "pointer" Ptr?))
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
         #:range-contracts (list
                            (and/c Stmt?
                                   (flat-named-contract
                                    "returns"
                                    (λ (x)
                                      ((Stmt-ret? x)))))))]
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

;; XXX Add a $lib for linking with Racket?
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

(define (compile&f u f)
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
               (close-input-port stdout)
               (thread-wait cpt)
               (get-output-string bs-stdout))))

(provide
 (contract-out
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

(define-syntax ($let* stx)
  (syntax-parse stx
    [(_ () . b)
     (syntax/loc stx
       ($begin . b))]
    [(_ ([ty n e] . more) . b)
     (syntax/loc stx
       ($let1 ([ty n e])
              ($let* more . b)))]))
(provide $let*)

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
             "-Wno-parentheses-equality"
             "-Wno-unused-parameter" "-Wno-unused-function"
             "-Werror" "-pedantic" "-std=c99" "-O3" "-march=native"
             "-fno-stack-protector" "-ffunction-sections" "-fdata-sections"
             "-fno-unwind-tables" "-fno-asynchronous-unwind-tables" "-fno-math-errno"
             "-fmerge-all-constants" "-fno-ident" "-fPIE" "-fPIC")
           ($ldflags '("-dead_strip") u)))
(provide $default-flags)
