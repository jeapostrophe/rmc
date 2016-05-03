#lang racket/base
(require rmc
         rmc/lib
         "fac.rkt")

(define*/rmc
  [cdouble
   ($proc ([U8 x]) U8
          ($ret ($+ x x)))]
  [cfac fac])

(module+ test
  (require chk)
  (chk (cdouble 5) 10
       (cfac 12) 479001600))
