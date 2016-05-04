#lang racket/base
(require rmc
         rmc/h/libc)

(define fac-rec
  ($proc ([U64 n]) U64
         ($if ($<= n ($v U64 0))
              ($ret ($v U64 1))
              ($ret ($* n
                        (fac-rec ($- n ($v U64 1))))))))
(define fac
  ($proc ([U64 n]) U64
         ($let1 ([U64 acc ($v U64 1)])
                ($while ($!= n ($v U64 0))
                        ($set! acc ($* acc n))
                        ($set! n ($- n ($v U64 1))))
                ($ret acc))))
(define main
  ($proc () S32
         (let ()
           (define (test-fac which fac)
             ($let1 ([U64 r ($v U64 0)])
                    ($for ([U32 i ($in-range ($v U32 10000))])
                          ($set! r (fac ($v U64 12))))
                    ($do ($printf ($v (format "~a r = %llu\n" which)) r))))
           ($begin (test-fac "iter" fac)
                   (test-fac " rec" fac-rec)
                   ($ret ($v S32 0))))))
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
