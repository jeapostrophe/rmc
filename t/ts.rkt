#lang racket/base
(require racket/match
         "../cl.rkt"
         "../h/libc.rkt"
         "fac.rkt")

(struct a-test* (stmt-f output))
(define-syntax-rule (a-test stmt output) (a-test* (λ () stmt) output))

(define (band a b) (and a b))
(define (bor a b) (or a b))

;; XXX test a recursive struct/union

(define Posn
  (Struct (list (cons 'x UI32)
                (cons 'y UI32))))
(define FloatInt
  (Union (list (cons 'f F64)
               (cons 'u UI64))))

(define TESTS
  (list
   (let ()
     (for/list ([t*s
                 (in-list
                  (list (cons UI8 1) (cons UI16 2) (cons UI32 4) (cons UI64 8)
                        (cons SI8 1) (cons SI16 2) (cons SI32 4) (cons SI64 8)
                        (cons F32 4) (cons F64 8)
                        (cons Char 1) (cons Size 8)
                        (cons (Ptr UI8) 8) (cons (Ptr UI64) 8)
                        (cons (Arr 4 UI8) 4) (cons (Arr 4 UI64) 32)
                        (cons (Ptr (Fun (list UI8 UI8) UI8)) 8)
                        (cons Bool 1)
                        (cons Posn 8) (cons FloatInt 8)))])
       (match-define (cons t s) t*s)
       (a-test ($do ($printf ($v "%lu\n") ($sizeof t)))
               (list (number->string s)))))

   (let ()
     (a-test ($begin ($do ($printf ($v "%lu\n") ($offsetof Posn 'x)))
                     ($do ($printf ($v "%lu\n") ($offsetof Posn 'y)))
                     ($do ($printf ($v "%lu\n") ($offsetof FloatInt 'f)))
                     ($do ($printf ($v "%lu\n") ($offsetof FloatInt 'u))))
             (list "0" "4" "0" "0")))

   (let ()
     (a-test ($let* ([Posn p])
                    ($set! ($@: p #:x) ($v UI32 1))
                    ($set! ($@: p #:y) ($v UI32 2))
                    ($do ($printf ($v "%u %u\n") ($@: p #:x) ($@: p #:y))))
             (list "1 2")))

   (let ()
     (a-test ($let* ([FloatInt f])
                    ($set! ($@: f #:f) ($v F64 3.14))
                    ($do ($printf ($v "%f %llu\n") ($@: f #:f) ($@: f #:u)))
                    ($set! ($@: f #:u) ($v UI64 3))
                    ($do ($printf ($v "%f %llu\n") ($@: f #:f) ($@: f #:u))))
             (list "3.140000 4614253070214989087"
                   "0.000000 3")))
   
   (let ()
     (define (cmp e)
       ($let* ([Bool i e])
              ($do ($printf ($v "%u\n")
                            ($ife i
                                  ($v UI8 1)
                                  ($v UI8 0))))))
     (a-test ($let* ([(Arr 4 UI8) a]
                     [(Arr 4 UI8) b])
                    ($set! ($aref a ($v UI8 0)) ($v UI8 1))
                    ($set! ($aref a ($v UI8 1)) ($v UI8 2))
                    ($set! ($aref a ($v UI8 2)) ($v UI8 3))
                    ($set! ($aref a ($v UI8 3)) ($v UI8 4))
                    ($do ($printf ($v "%u\n") ($@: a ($v UI8 2))))
                    ($do ($printf ($v "%u\n") ($@ a)))
                    ($begin (cmp ($!= a b))
                            (cmp ($!= a a))
                            (cmp ($== a b))
                            (cmp ($== a a))))
             (list "3"
                   "1"
                   "1" "0"
                   "0" "1")))
   (let ()
     (define add2
       ($proc ([UI8 a]) UI8
              ($ret ($+ a ($v UI8 2)))))
     (define add3
       ($proc ([UI8 a]) UI8
              ($ret ($+ a ($v UI8 3)))))
     (a-test
      ($let* ([(Ptr (Fun (list UI8) UI8))
               f ($& ($dref add2))]
              [(Ptr (Fun (list UI8) UI8))
               g ($& ($dref add3))]
              [UI8 two ($v UI8 2)])
             ($do ($printf ($v "%u\n") (add2 two)))
             ($do ($printf ($v "%u\n") ($app ($@ f) two)))
             ($do ($printf ($v "%u\n") ($app f two)))
             ($do ($printf ($v "%u\n") (add3 two)))
             ($do ($printf ($v "%u\n") ($app ($@ g) two)))
             ($do ($printf ($v "%u\n") ($app g two)))
             ($do ($printf ($v "%d\n")
                           ($ife ($== f f) ($v UI8 1) ($v UI8 0))))
             ($do ($printf ($v "%d\n")
                           ($ife ($== f g) ($v UI8 1) ($v UI8 0)))))
      (list "4" "4" "4"
            "5" "5" "5"
            "1" "0")))

   (a-test ($let* ([UI8 i ($v UI8 32)]
                   [UI8 j ($v UI8 32)]
                   [UI8 k ($v UI8 33)]
                   [(Ptr UI8) ip1 ($& i)]
                   [(Ptr UI8) ip2 ($& i)]
                   [(Ptr UI8) jp ($& j)]
                   [(Ptr UI8) kp ($& k)])
                  ($do ($printf ($v "%d\n")
                                ($ife ($== ip1 ip2) ($v UI8 1) ($v UI8 0))))
                  ($do ($printf ($v "%d\n")
                                ($ife ($!= ip1 ip2) ($v UI8 1) ($v UI8 0))))
                  ($do ($printf ($v "%d\n")
                                ($ife ($== ip1 jp) ($v UI8 1) ($v UI8 0))))
                  ($do ($printf ($v "%d\n")
                                ($ife ($!= ip1 jp) ($v UI8 1) ($v UI8 0))))
                  ($do ($printf ($v "%d\n")
                                ($ife ($== ($@ ip1) ($@ jp)) ($v UI8 1) ($v UI8 0))))
                  ($do ($printf ($v "%d\n")
                                ($ife ($== ($@ ip1) ($@ kp)) ($v UI8 1) ($v UI8 0)))))
           (list "1" "0" "0" "1" "1" "0"))

   (for/list ([$op (in-list (list $and $or))]
              [op (in-list (list band bor))])
     (for*/list ([x (in-list '(#t #f))]
                 [y (in-list '(#t #f))])
       (a-test ($do ($printf ($v "%u\n")
                             ($ife ($op ($v x) ($v y))
                                   ($v UI8 42)
                                   ($v UI8 32))))
               (list (if (op x y) 42 32)))))
   (let ()
     (define (test-$op X Y TY TYp $op op
                       #:CAST [C #f]
                       #:F [F number->string])
       (a-test ($let* ([TY x ($v TY X)]
                       [TY y ($v TY Y)]
                       [TY z ($op x y)])
                      ($do ($printf ($v (string-append TYp "\n"))
                                    (if C
                                        ($cast C z)
                                        z))))
               (list (F (op X Y)))))
     (list
      (for/list ([$op (in-list (list $% $band $bior $bxor
                                     $bshl $bshr))]
                 [op (in-list (list modulo bitwise-and bitwise-ior bitwise-xor
                                    arithmetic-shift
                                    (λ (x y) (arithmetic-shift x (* -1 y)))))])
        (for/list ([TY (in-list (list UI8 UI16 UI32 UI64))]
                   [TYp (in-list (list "%hhu" "%hu" "%u" "%llu"))])
          (test-$op 13 4 TY TYp $op op)))
      (let ()
        (for/list ([TY (in-list (list F32 F64))]
                   [X (in-list (list 8f0 8.0))]
                   [Y (in-list (list 6f0 6.0))])
          (for/list ([$op (in-list (list $* $- $+ $/))]
                     [op (in-list (list * - + /))])
            (test-$op X Y TY "%.4f" $op op
                      #:CAST (if (single-flonum? X) F64 #f)
                      #:F (λ (x) (real->decimal-string x 4))))))
      (let ()
        (define X 8)
        (define Y 6)
        (for/list ([TY (in-list (list UI8 UI16 UI32 UI64
                                      SI8 SI16 SI32 SI64))]
                   [TYp (in-list (list "%hhu" "%hu" "%u" "%llu"
                                       "%hhd" "%hd" "%d" "%lld"))])
          (for/list ([$op (in-list (list $* $- $+ $/))]
                     [op (in-list (list * - + quotient))])
            (test-$op X Y TY TYp $op op))))))
   (a-test ($do ($printf ($v "%u\n")
                         ($seal 'MPH ($v UI32 19))))
           '("19"))
   (a-test ($do ($printf ($v "%u\n")
                         ($+ ($v UI32 1) ($unseal 'MPH ($seal 'MPH ($v UI32 19))))))
           '("20"))
   (let ()
     (define (!= x y) (not (= x y)))
     (define == =)
     (for/list ([<= (in-list (list <= < > >=
                                   != ==))]
                [$<= (in-list (list $<= $< $> $>= $!= $==))])
       (define (<=-test TY L R)
         (a-test ($let* ([UI8 i ($v UI8 0)]
                         [TY l ($v TY L)])
                        ($when ($<= l ($v TY R))
                               ($set! i ($v UI8 1)))
                        ($do ($printf ($v "%d\n") i)))
                 (list (number->string (if (<= L R) 1 0)))))
       (cons
        (list (<=-test F64 4.0 16.0)
              (<=-test F64 16.0 16.0)
              (<=-test F64 16.0 4.0))
        (for/list ([TY (in-list (list UI8 UI16 UI32 UI64
                                      SI8 SI16 SI32 SI64))])
          (list (<=-test TY 4 16)
                (<=-test TY 16 16)
                (<=-test TY 16 4))))))
   (let ()
     (define (bneg-test TY fmt max)
       (a-test ($let1 ([TY i ($bneg ($v TY 5))])
                      ($do ($printf ($v fmt) i)))
               (list (number->string (- (- max 1) 5)))))
     (list (bneg-test  UI8 "%hhu\n" (expt 2  8))
           (bneg-test UI16  "%hu\n" (expt 2 16))
           (bneg-test UI32   "%u\n" (expt 2 32))
           (bneg-test UI64 "%llu\n" (expt 2 64))))
   (a-test ($do ($printf ($v "%d\n") ($neg ($v SI32 5))))
           '("-5"))
   (a-test ($do ($printf ($v "%d\n") ($neg ($v SI32 -5))))
           '("5"))
   (a-test ($do ($printf ($v "%llu\n") (fac ($v UI64 12))))
           '("479001600"))
   (a-test ($let1 ([String fmt ($v "Hello World!\n")])
                  ($do ($printf ($v "%s") fmt)))
           '("Hello World!"))
   (let ()
     (define (bool-test B)
       (a-test ($let1 ([Bool b ($v B)])
                      ($if ($! b)
                           ($do ($printf ($v "Yes\n")))
                           ($do ($printf ($v "No\n")))))
               (list (if (not B) "Yes" "No"))))
     (list
      (bool-test #t)
      (bool-test #f)))
   (let ()
     (define (bool-test B)
       (a-test ($let1 ([Bool b ($v B)])
                      ($if b
                           ($do ($printf ($v "Yes\n")))
                           ($do ($printf ($v "No\n")))))
               (list (if B "Yes" "No"))))
     (list
      (bool-test #t)
      (bool-test #f)))
   (a-test ($let1 ([F64 i ($v F64 3.14)])
                  ($do ($printf ($v "%f\n") i)))
           '("3.140000"))
   (a-test $nop
           '())
   (a-test ($let1 ([UI8 i ($v UI8 0)])
                  ($when ($!= i ($v UI8 0))
                         ($set! i ($v UI8 1)))
                  ($do ($printf ($v "when %d\n") i)))
           '("when 0"))
   (a-test ($let1 ([UI8 i ($v UI8 0)])
                  ($unless ($!= i ($v UI8 0))
                           ($set! i ($v UI8 1)))
                  ($do ($printf ($v "unless %d\n") i)))
           '("unless 1"))
   (a-test ($begin
            ($do ($printf ($v "for:")))
            ($for ([UI8 i ($in-range ($v UI8 10))])
                  ($do ($printf ($v " %d") i)))
            ($do ($printf ($v "\n"))))
           '("for: 0 1 2 3 4 5 6 7 8 9"))
   (a-test ($let* () ($do ($printf ($v "nothing\n"))))
           '("nothing"))
   (a-test ($let* ([UI8 i ($v UI8 12)])
                  ($do ($printf ($v "%d\n") i)))
           '("12"))
   (a-test ($let* ([UI8 i ($v UI8 12)]
                   [UI8 j ($v UI8 24)])
                  ($do ($printf ($v "%d %d\n") i j)))
           '("12 24"))
   (a-test ($let* ([UI8 i ($v UI8 12)]
                   [UI8 i ($v UI8 24)])
                  ($do ($printf ($v "%d\n") i)))
           '("24"))))

(module+ test
  (require "../check.rkt"
           racket/string)

  (define (walk-tree f tt)
    (cond [(null? tt) (void)]
          [(pair? tt)
           (walk-tree f (car tt))
           (walk-tree f (cdr tt))]
          [else
           (f tt)]))

  (define (run-a-test t)
    (define main
      ($proc () SI32
             ($begin
              ((a-test*-stmt-f t))
              ($ret ($v SI32 0)))))

    (define this
      ($default-flags ($exe main)))

    (define emit? #f)
    (with-handlers ([exn:fail?
                     (λ (x)
                       (set! emit? #t)
                       (with-check-details (['fail "compilation failed"])
                         (check #f)))])
      (define actual-out (string-split (run&capture this) "\n"))
      (define expected-out (a-test*-output t))

      (with-check-details ([check-inform (λ () (set! emit? #t))])
        (check (length actual-out) (length expected-out))
        (for ([a (in-list actual-out)]
              [e (in-list expected-out)])
          (cond
            [(string? e)
             (check a e)]
            [(number? e)
             (check (string->number a) e)]
            [else
             (with-check-details (['fail "invalid expected"]
                                  ['v (format "~e" e)])
               (check #f))]))))

    (when emit?
      (emit! this)))

  (walk-tree run-a-test TESTS))
