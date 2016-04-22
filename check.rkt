#lang racket/base
(require (for-syntax racket/base
                     syntax/parse)
         rackunit/log
         racket/match
         racket/port)

(define check-inform (gensym))

(define-syntax (check stx)
  (syntax-parse stx
    [(_ e:expr)
     (syntax/loc stx
       (check e #t))]
    [(_ e:expr a:expr)
     (syntax/loc stx
       (check equal? e a))]
    [(_ f:expr e:expr a:expr)
     (syntax/loc stx
       (check* f (λ () e) (λ () a)))]))

(define current-check-detail (make-parameter null))

(define-syntax (with-check-details stx)
  (syntax-parse stx
    [(_ ([k:expr v:expr] ...) . b)
     (syntax/loc stx
       (parameterize ([current-check-detail
                       (append (current-check-detail)
                               (list (cons k v) ...))])
         . b))]))

(define (exception-details x)
  (list (cons 'exn
              (with-output-to-string
                (λ ()
                  ((error-display-handler) (exn-message x) x))))))

(define (check* the-equal? actual-f expected-f)
  (define (fail! msg keys)
    (eprintf "failure: ~a\n" msg)
    (for ([kv (in-list (append (current-check-detail) keys))])
      (match-define (cons k v) kv)
      (match k
        [(== check-inform)
         (v)]
        [_
         (eprintf "~a: ~a\n" k v)]))
    (test-log! #f))
  (define actual
    (with-handlers
      ([exn:fail?
        (λ (x)
          (fail! "actual raised exception"
                 (exception-details x)))])
      (actual-f)))
  (define expected
    (with-handlers
      ([exn:fail?
        (λ (x)
          (fail! "expected raised exception"
                 (exception-details x)))])
      (expected-f)))
  (cond
    [(the-equal? actual expected)
     (test-log! #t)]
    [else
     (fail! "not comparable"
            (list (cons 'actual actual)
                  (cons 'expected expected)))]))

(provide check
         with-check-details
         check-inform)
