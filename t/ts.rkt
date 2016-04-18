#lang racket/base
(require "../cl.rkt"
         "../h/libc.rkt")

(define main
  ($proc () SI32
         ($ret ($v SI32 0))))

(define this
  ($default-flags ($exe main)))

(module+ test
  (require rackunit
           racket/string)
  (emit! this)
  (check-equal? (string-split (run&capture this) "\n")
                (list)))
