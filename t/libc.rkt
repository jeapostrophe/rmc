#lang racket/base
(require "../t.rkt"
         "../cl.rkt"
         "../h/libc.rkt")

(module+ test
  (cchk ($begin ($do ($printf ($v "test")))
                ($unless ($== ($v S32 0) (fflush stdin))
                         ($do ($printf ($v "error!")))))
        '("test")))
