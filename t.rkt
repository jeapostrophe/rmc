#lang racket/base
(require chk
         racket/string
         "main.rkt")

(define (cchk stmt output)
  (define main
    ($proc () S32
           ($begin
            stmt
            ($ret ($v S32 0)))))

  (define this
    ($default-flags ($exe main)))

  (define emit? #f)
  (with-handlers ([exn:fail?
                   (λ (x)
                     (set! emit? #t)
                     (with-chk (['fail "compilation failed"])
                       (chk #f)))])
    (define actual-out (string-split (run&capture this) "\n"))
    (define expected-out output)

    (with-chk ([chk-inform! (λ (_) (set! emit? #t))])
      (chk (length actual-out) (length expected-out))
      (for ([a (in-list actual-out)]
            [e (in-list expected-out)])
        (cond
          [(string? e)
           (chk a e)]
          [(number? e)
           (chk (string->number a) e)]
          [else
           (with-chk (['fail "invalid expected"]
                      ['v (format "~e" e)])
             (chk #f))]))))

  (when emit?
    (emit! this)))

(provide cchk)
