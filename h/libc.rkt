#lang racket/base
(require "../cl.rkt")

(define <stdio.h> (CHeader '() '() '() "<stdio.h>" '()))
(define $printf ($extern <stdio.h> "printf" Any))

(define FILE (Extern <stdio.h> (Opaque "FILE")))
(define stdin ($extern <stdio.h> "stdin" (Ptr FILE)))
(define stdout ($extern <stdio.h> "stdout" (Ptr FILE)))
(define stderr ($extern <stdio.h> "stderr" (Ptr FILE)))

(define fflush
  ($extern <stdio.h> "fflush"
           (Fun (list (Ptr FILE)) SI32)))

(provide (all-defined-out))
