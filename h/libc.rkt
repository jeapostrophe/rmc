#lang racket/base
(require rmc
         rmc/h)

(define-rmc/header
  (H "<stdio.h>")
  (T FILE)
  (T fpos_t)
  (V stderr FILE*)
  (V stdin FILE*)
  (V stdout FILE*)
  (F clearerr FILE* -> Void)
  (F fclose FILE* -> S32)
  (F fdopen S32 String -> FILE*)
  (F feof FILE* -> S32)
  (F ferror FILE* -> S32)
  (F fflush FILE* -> S32)
  (F fgetc FILE* -> S32)
  (F fgetpos FILE* fpos_t* -> S32)
  ;; XXX fill out
  (V [$printf printf])
  (V [$fprintf fprintf]))
