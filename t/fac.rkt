#lang racket/base
(require "../cl.rkt"
         "../h/libc.rkt")

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

(provide fac)

(module+ test
  (require rackunit
           racket/string)
  (emit! this)
  (check-equal? (string-split (run&capture this) "\n")
                (list "iter r = 479001600"
                      " rec r = 479001600")))
