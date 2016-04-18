#lang racket/base
(require "../cl.rkt"
         "../h/libc.rkt")

;; XXX Any Size Char Void UI8 UI16 UI32 UI32 UI64 SI8 SI16 SI32 SI64
;; F32 F64 Ptr Fun Seal Bool String

;; XXX $dref $! $neg $bneg $<= $< $!= $* $- $+

;; XXX $if $%while $ret/ty

;; XXX $extern $%proc

;; XXX $app $v $while $let*

(define test-stmts
  ($proc () Void
         ($begin
          ;; $nop
          $nop
          ;; $when
          ($let1 ([UI8 i ($v UI8 0)])
                 ($when ($!= i ($v UI8 0))
                        ($set! i ($v UI8 1)))
                 ($do ($printf ($v "when %d\n") i)))
          ;; $unless
          ($let1 ([UI8 i ($v UI8 0)])
                 ($unless ($!= i ($v UI8 0))
                          ($set! i ($v UI8 1)))
                 ($do ($printf ($v "unless %d\n") i)))
          ;; $for
          ($begin
           ($do ($printf ($v "for:")))
           ($for ([UI8 i ($in-range ($v UI8 10))])
                 ($do ($printf ($v " %d") i)))
           ($do ($printf ($v "\n"))))
          ($return))))

(define main
  ($proc () SI32
         ($begin
          ($do (test-stmts))
          ($ret ($v SI32 0)))))

(define this
  ($default-flags ($exe main)))

(module+ test
  (require rackunit
           racket/string)
  (emit! this)
  (check-equal? (string-split (run&capture this) "\n")
                (list "when 0"
                      "unless 1"
                      "for: 0 1 2 3 4 5 6 7 8 9")))
