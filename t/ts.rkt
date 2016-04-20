#lang racket/base
(require "../cl.rkt"
         "../h/libc.rkt"
         racket/list)

(struct a-test* (stmt-f output))
(define-syntax-rule (a-test stmt output) (a-test* (λ () stmt) output))
(define (run-tests ts)
  (foldr (λ (a b) ($seq a b)) $nop (map (λ (x) ((a-test*-stmt-f x))) ts)))

;; XXX Any Size Char Void UI8 UI16 UI32 UI32 UI64 SI8 SI16 SI32 SI64
;; F32 F64 Ptr Fun Seal Bool String

;; XXX $dref $! $neg $bneg $<= $< $!= $* $- $+

;; XXX $if $%while $ret/ty

;; XXX $extern $%proc

;; XXX $app $v $while $let*

(define TESTS
  (list
   (a-test ($let1 ([F64 i ($v F64 3.14)])
                  ($do ($printf ($v "%f\n") i)))
           (list "3.140000"))
   (a-test $nop
           (list))
   (a-test ($let1 ([UI8 i ($v UI8 0)])
                  ($when ($!= i ($v UI8 0))
                         ($set! i ($v UI8 1)))
                  ($do ($printf ($v "when %d\n") i)))
           (list "when 0"))
   (a-test ($let1 ([UI8 i ($v UI8 0)])
                  ($unless ($!= i ($v UI8 0))
                           ($set! i ($v UI8 1)))
                  ($do ($printf ($v "unless %d\n") i)))
           (list "unless 1"))
   (a-test ($begin
            ($do ($printf ($v "for:")))
            ($for ([UI8 i ($in-range ($v UI8 10))])
                  ($do ($printf ($v " %d") i)))
            ($do ($printf ($v "\n"))))
           (list "for: 0 1 2 3 4 5 6 7 8 9"))))

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
   (for/fold ([out (string-split (run&capture this) "\n")])
             ([t (in-list TESTS)])
     (for/fold ([out out])
               ([o (in-list (a-test*-output t))])
       (check-equal? (first out) o)
       (rest out)))))
