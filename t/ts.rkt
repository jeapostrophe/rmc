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

;; XXX Any Size Char Void UI8 UI16 UI32 UI32 UI64 SI8 SI16 SI32 SI64
;; F32 F64 Ptr Fun

;; XXX $<= $< $!= $* $- $+

(define TESTS
  (list
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
             (check-equal? (first out) o)
             (rest out)]
            [else
             out]))]))))
