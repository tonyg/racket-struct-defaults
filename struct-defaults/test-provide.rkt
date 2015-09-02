#lang racket/base

(module+ test
  (require rackunit)
  (require racket/match)

  (module test-struct-out-with-defaults racket/base
    (require "main.rkt")
    (provide (struct-out/defaults [make-foo foo]))
    (struct foo (bar zot) #:transparent)
    (define-struct-defaults make-foo foo ([foo-bar 'bar] #:zot foo-zot)))

  (require 'test-struct-out-with-defaults)

  (check-true (foo? (foo #:zot 'zot)))
  (check-true (match (foo #:zot 'zot)
                [(foo #:zot _) #t]
                [_ #f]))
  (check-true (match (foo #:zot 'zot)
                [(foo 'bar #:zot _) #t]
                [_ #f]))
  (check-true (match (foo #:zot 'zot 'qux)
                [(foo 'qux #:zot _) #t]
                [_ #f]))
  (check-true (match (foo #:zot 'zot)
                [(foo 'qux #:zot _) #f]
                [_ #t]))
  )
