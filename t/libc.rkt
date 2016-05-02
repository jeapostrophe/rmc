#lang racket/base
(require "../t.rkt"
         "../cl.rkt"
         "../h/libc.rkt")

(module+ test
  (a-test ($begin ($do ($printf ($v "test")))
                  ($unless ($== ($v SI32 0) (fflush stdin))
                           ($do ($printf ($v "error!")))))
          '("test")))
