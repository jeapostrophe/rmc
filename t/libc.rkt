#lang racket/base
(require rmc
         rmc/h/libc
         rmc/t)

(module+ test
  (cchk ($begin ($do ($printf ($v "test")))
                ($unless ($== ($v S32 0) (fflush stdin))
                         ($do ($printf ($v "error!")))))
        '("test")))
