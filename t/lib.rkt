#lang racket/base
(require "../cl.rkt"
         "../lib.rkt"
         "fac.rkt")

(define-cl-lib
  [cdouble
   ($proc ([U8 x]) U8
          ($ret ($+ x x)))]
  [cfac fac])

(module+ test
  (require chk)
  (chk (cdouble 5) 10
       (cfac 12) 479001600))
