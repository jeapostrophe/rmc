#lang racket/base
(module+ test
  (require racket/match
           "../cl.rkt"
           "../h/libc.rkt"
           "../t.rkt"
           "fac.rkt")

  (define (band a b) (and a b))
  (define (bor a b) (or a b))

  (define Posn
    (Struct [x U32] [y U32]))
  (define FloatInt
    (Union [f F64] [u U64]))

  (define ListOfU8
    (Struct [next (Ptr ListOfU8)] [data U8] [padding (Arr 7 U8)]))

  (cchk ($let* ([ListOfU8 l1]
                [ListOfU8 l2]
                [ListOfU8 l3])
               ($set! ($@ l1 #:data) ($v U8 1))
               ($set! ($@ l1 #:next) ($& l2))
               ($set! ($@ l2 #:data) ($v U8 2))
               ($set! ($@ l2 #:next) ($& l3))
               ($set! ($@ l3 #:data) ($v U8 3))
               ($set! ($@ l3 #:next) $NULL)
               ($do ($printf ($v "%u %u %u\n")
                             ($@ l1 #:data)
                             ($@ l1 #:next -> #:data)
                             ($@ l1 #:next -> #:next -> #:data))))
        (list "1 2 3"))

  (let ()
    (for ([t*s
           (in-list
            (list (cons U8 1) (cons U16 2) (cons U32 4) (cons U64 8)
                  (cons S8 1) (cons S16 2) (cons S32 4) (cons S64 8)
                  (cons F32 4) (cons F64 8)
                  (cons Char 1) (cons Size 8)
                  (cons (Ptr U8) 8) (cons (Ptr U64) 8)
                  (cons (Arr 4 U8) 4) (cons (Arr 4 U64) 32)
                  (cons (Ptr (Fun (list U8 U8) U8)) 8)
                  (cons Bool 1)
                  (cons Posn 8) (cons FloatInt 8)))])
      (match-define (cons t s) t*s)
      (cchk ($do ($printf ($v "%lu\n") ($sizeof t)))
            (list (number->string s)))))

  (let ()
    (cchk ($begin ($do ($printf ($v "%lu\n") ($offsetof Posn 'x)))
                  ($do ($printf ($v "%lu\n") ($offsetof Posn 'y)))
                  ($do ($printf ($v "%lu\n") ($offsetof FloatInt 'f)))
                  ($do ($printf ($v "%lu\n") ($offsetof FloatInt 'u))))
          (list "0" "4" "0" "0")))

  (let ()
    (cchk ($let* ([Posn p])
                 ($set! ($@ p #:x) ($v U32 1))
                 ($set! ($@ p #:y) ($v U32 2))
                 ($do ($printf ($v "%u %u\n") ($@ p #:x) ($@ p #:y))))
          (list "1 2")))

  (let ()
    (cchk ($let* ([FloatInt f])
                 ($set! ($@ f #:f) ($v F64 3.14))
                 ($do ($printf ($v "%f %llu\n") ($@ f #:f) ($@ f #:u)))
                 ($set! ($@ f #:u) ($v U64 3))
                 ($do ($printf ($v "%f %llu\n") ($@ f #:f) ($@ f #:u))))
          (list "3.140000 4614253070214989087"
                "0.000000 3")))

  (let ()
    (define (cmp e)
      ($let* ([Bool i e])
             ($do ($printf ($v "%u\n")
                           ($ife i
                                 ($v U8 1)
                                 ($v U8 0))))))
    (cchk ($let* ([(Arr 4 U8) a]
                  [(Arr 4 U8) b])
                 ($set! ($aref a ($v U8 0)) ($v U8 1))
                 ($set! ($aref a ($v U8 1)) ($v U8 2))
                 ($set! ($aref a ($v U8 2)) ($v U8 3))
                 ($set! ($aref a ($v U8 3)) ($v U8 4))
                 ($do ($printf ($v "%u\n") ($@ a ($v U8 2))))
                 ($do ($printf ($v "%u\n") ($@ * a)))
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
      ($proc ([U8 a]) U8
             ($ret ($+ a ($v U8 2)))))
    (define add3
      ($proc ([U8 a]) U8
             ($ret ($+ a ($v U8 3)))))
    (cchk
     ($let* ([(Ptr (Fun (list U8) U8))
              f ($& add2)]
             [(Ptr (Fun (list U8) U8))
              g ($& add3)]
             [U8 two ($v U8 2)])
            ($do ($printf ($v "%u\n") (add2 two)))
            ($do ($printf ($v "%u\n") ($app ($@ * f) two)))
            ($do ($printf ($v "%u\n") ($app f two)))
            ($do ($printf ($v "%u\n") (add3 two)))
            ($do ($printf ($v "%u\n") ($app ($@ * g) two)))
            ($do ($printf ($v "%u\n") ($app g two)))
            ($do ($printf ($v "%d\n")
                          ($ife ($== f f) ($v U8 1) ($v U8 0))))
            ($do ($printf ($v "%d\n")
                          ($ife ($== f g) ($v U8 1) ($v U8 0)))))
     (list "4" "4" "4"
           "5" "5" "5"
           "1" "0")))

  (cchk ($let* ([U8 i ($v U8 32)]
                [U8 j ($v U8 32)]
                [U8 k ($v U8 33)]
                [(Ptr U8) ip1 ($& i)]
                [(Ptr U8) ip2 ($& i)]
                [(Ptr U8) jp ($& j)]
                [(Ptr U8) kp ($& k)])
               ($do ($printf ($v "%d\n")
                             ($ife ($== ip1 ip2) ($v U8 1) ($v U8 0))))
               ($do ($printf ($v "%d\n")
                             ($ife ($!= ip1 ip2) ($v U8 1) ($v U8 0))))
               ($do ($printf ($v "%d\n")
                             ($ife ($== ip1 jp) ($v U8 1) ($v U8 0))))
               ($do ($printf ($v "%d\n")
                             ($ife ($!= ip1 jp) ($v U8 1) ($v U8 0))))
               ($do ($printf ($v "%d\n")
                             ($ife ($== ($@ * ip1) ($@ * jp))
                                   ($v U8 1) ($v U8 0))))
               ($do ($printf ($v "%d\n")
                             ($ife ($== ($@ * ip1) ($@ * kp))
                                   ($v U8 1) ($v U8 0)))))
        (list "1" "0" "0" "1" "1" "0"))

  (for ([$op (in-list (list $and $or))]
        [op (in-list (list band bor))])
    (for* ([x (in-list '(#t #f))]
           [y (in-list '(#t #f))])
      (cchk ($do ($printf ($v "%u\n")
                          ($ife ($op ($v x) ($v y))
                                ($v U8 42)
                                ($v U8 32))))
            (list (if (op x y) 42 32)))))
  (let ()
    (define (test-$op X Y TY TYp $op op
                      #:CAST [C #f]
                      #:F [F number->string])
      (cchk ($let* ([TY x ($v TY X)]
                    [TY y ($v TY Y)]
                    [TY z ($op x y)])
                   ($do ($printf ($v (string-append TYp "\n"))
                                 (if C
                                     ($cast C z)
                                     z))))
            (list (F (op X Y)))))
    (for ([$op (in-list (list $% $band $bior $bxor
                              $bshl $bshr))]
          [op (in-list (list modulo bitwise-and bitwise-ior bitwise-xor
                             arithmetic-shift
                             (λ (x y) (arithmetic-shift x (* -1 y)))))])
      (for ([TY (in-list (list U8 U16 U32 U64))]
            [TYp (in-list (list "%hhu" "%hu" "%u" "%llu"))])
        (test-$op 13 4 TY TYp $op op)))
    (let ()
      (for ([TY (in-list (list F32 F64))]
            [X (in-list (list 8f0 8.0))]
            [Y (in-list (list 6f0 6.0))])
        (for ([$op (in-list (list $* $- $+ $/))]
              [op (in-list (list * - + /))])
          (test-$op X Y TY "%.4f" $op op
                    #:CAST (if (single-flonum? X) F64 #f)
                    #:F (λ (x) (real->decimal-string x 4))))))
    (let ()
      (define X 8)
      (define Y 6)
      (for ([TY (in-list (list U8 U16 U32 U64
                               S8 S16 S32 S64))]
            [TYp (in-list (list "%hhu" "%hu" "%u" "%llu"
                                "%hhd" "%hd" "%d" "%lld"))])
        (for ([$op (in-list (list $* $- $+ $/))]
              [op (in-list (list * - + quotient))])
          (test-$op X Y TY TYp $op op)))))
  (cchk ($do ($printf ($v "%u\n")
                      ($seal 'MPH ($v U32 19))))
        '("19"))
  (cchk ($do ($printf ($v "%u\n")
                      ($+ ($v U32 1) ($unseal 'MPH ($seal 'MPH ($v U32 19))))))
        '("20"))
  (let ()
    (define (!= x y) (not (= x y)))
    (define == =)
    (for ([<= (in-list (list <= < > >=
                             != ==))]
          [$<= (in-list (list $<= $< $> $>= $!= $==))])
      (define (<=-test TY L R)
        (cchk ($let* ([U8 i ($v U8 0)]
                      [TY l ($v TY L)])
                     ($when ($<= l ($v TY R))
                            ($set! i ($v U8 1)))
                     ($do ($printf ($v "%d\n") i)))
              (list (number->string (if (<= L R) 1 0)))))
      (<=-test F64 4.0 16.0)
      (<=-test F64 16.0 16.0)
      (<=-test F64 16.0 4.0)
      (for ([TY (in-list (list U8 U16 U32 U64
                               S8 S16 S32 S64))])
        (<=-test TY 4 16)
        (<=-test TY 16 16)
        (<=-test TY 16 4))))
  (let ()
    (define (bneg-test TY fmt max)
      (cchk ($let1 ([TY i ($bneg ($v TY 5))])
                   ($do ($printf ($v fmt) i)))
            (list (number->string (- (- max 1) 5)))))
    (bneg-test  U8 "%hhu\n" (expt 2  8))
    (bneg-test U16  "%hu\n" (expt 2 16))
    (bneg-test U32   "%u\n" (expt 2 32))
    (bneg-test U64 "%llu\n" (expt 2 64)))
  (cchk ($do ($printf ($v "%d\n") ($neg ($v S32 5))))
        '("-5"))
  (cchk ($do ($printf ($v "%d\n") ($neg ($v S32 -5))))
        '("5"))
  (cchk ($do ($printf ($v "%llu\n") (fac ($v U64 12))))
        '("479001600"))
  (cchk ($let1 ([String fmt ($v "Hello World!\n")])
               ($do ($printf ($v "%s") fmt)))
        '("Hello World!"))
  (let ()
    (define (bool-test B)
      (cchk ($let1 ([Bool b ($v B)])
                   ($if ($! b)
                        ($do ($printf ($v "Yes\n")))
                        ($do ($printf ($v "No\n")))))
            (list (if (not B) "Yes" "No"))))
    (bool-test #t)
    (bool-test #f))
  (let ()
    (define (bool-test B)
      (cchk ($let1 ([Bool b ($v B)])
                   ($if b
                        ($do ($printf ($v "Yes\n")))
                        ($do ($printf ($v "No\n")))))
            (list (if B "Yes" "No"))))
    (bool-test #t)
    (bool-test #f))
  (cchk ($let1 ([F64 i ($v F64 3.14)])
               ($do ($printf ($v "%f\n") i)))
        '("3.140000"))
  (cchk $nop
        '())
  (cchk ($let1 ([U8 i ($v U8 0)])
               ($when ($!= i ($v U8 0))
                      ($set! i ($v U8 1)))
               ($do ($printf ($v "when %d\n") i)))
        '("when 0"))
  (cchk ($let1 ([U8 i ($v U8 0)])
               ($unless ($!= i ($v U8 0))
                        ($set! i ($v U8 1)))
               ($do ($printf ($v "unless %d\n") i)))
        '("unless 1"))
  (cchk ($begin
         ($do ($printf ($v "for:")))
         ($for ([U8 i ($in-range ($v U8 10))])
               ($do ($printf ($v " %d") i)))
         ($do ($printf ($v "\n"))))
        '("for: 0 1 2 3 4 5 6 7 8 9"))
  (cchk ($let* () ($do ($printf ($v "nothing\n"))))
        '("nothing"))
  (cchk ($let* ([U8 i ($v U8 12)])
               ($do ($printf ($v "%d\n") i)))
        '("12"))
  (cchk ($let* ([U8 i ($v U8 12)]
                [U8 j ($v U8 24)])
               ($do ($printf ($v "%d %d\n") i j)))
        '("12 24"))
  (cchk ($let* ([U8 i ($v U8 12)]
                [U8 i ($v U8 24)])
               ($do ($printf ($v "%d\n") i)))
        '("24")))
