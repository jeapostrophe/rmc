#lang racket/base
(require "../cl.rkt"
         "../h/libc.rkt"
         "fac.rkt"
         racket/list)

(struct a-test* (stmt-f output))
(define-syntax-rule (a-test stmt output) (a-test* (λ () stmt) output))
(define (run-tests tt)
  (let loop ([tt tt])
    (cond
      [(null? tt) $nop]
      [(pair? tt) ($seq (loop (car tt))
                        (loop (cdr tt)))]
      [else
       ((a-test*-stmt-f tt))])))

;; XXX test function pointers
;; XXX test $== and $!= on Ptrs

(define-syntax-rule (FUN x)
  (λ (a b) (x a b)))

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

(define TESTS
  (list
   (for/list ([$op (in-list (list (FUN $and) (FUN $or)))]
              [op (in-list (list (FUN and) (FUN or)))])
     (for*/list ([x (in-list '(#t #f))]
                 [y (in-list '(#t #f))])
       (a-test ($do ($printf ($v "%u\n")
                             ($ife ($op ($v x) ($v y))
                                   ($v UI8 42)
                                   ($v UI8 32))))
               (list (if (op x y) 42 32)))))   
   (for/list ([$op (in-list (list (FUN $%) (FUN $band) (FUN $bior) (FUN $bxor)
                                  (FUN $bshl) (FUN $bshr)))]
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
       (for/list ([$op (in-list (list (FUN $*) (FUN $-) (FUN $+) (FUN $/)))]
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
       (for/list ([$op (in-list (list (FUN $*) (FUN $-) (FUN $+) (FUN $/)))]
                  [op (in-list (list * - + quotient))])
         (test-$op X Y TY TYp $op op))))
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
                [$<= (in-list (list (FUN $<=) (FUN $<) (FUN $>) (FUN $>=)
                                    (FUN $!=) (FUN $==)))])
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

(define main
  ($proc () SI32
         ($begin
          (run-tests TESTS)
          ($ret ($v SI32 0)))))

(define this
  ($default-flags ($exe main)))

(module+ test
  (require rackunit
           racket/string)
  (emit! this)

  (void
   (let loop ([out (string-split (run&capture this) "\n")]
              [tt TESTS])
     (cond
       [(null? tt) out]
       [(pair? tt) (loop (loop out (car tt))
                         (cdr tt))]
       [else
        (for/fold ([out out])
                  ([o (in-list (a-test*-output tt))])
          (check-pred pair? out)
          (cond
            [(pair? out)
             (define fo (first out))
             (cond
               [(string? o)
                (check-equal? fo o)]
               [(number? o)
                (check-equal? (string->number fo) o)]
               [else
                (error 'test)])
             (rest out)]
            [else
             out]))]))))
