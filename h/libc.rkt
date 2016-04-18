#lang racket/base
(require "../cl.rkt")

(define <stdio.h> (CHeader '() '() '() "<stdio.h>" '()))
(define $printf ($extern <stdio.h> "printf" Any))

(provide (all-defined-out))
