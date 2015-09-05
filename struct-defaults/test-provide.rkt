#lang racket/base

(module+ test
  (require rackunit)
  (require racket/match)
  (require "main.rkt")

  (module test-struct-out-with-defaults racket/base
    (require "main.rkt")
    (provide (rename-out [foo orig-foo]))
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

  (define-struct-defaults foo1 orig-foo () #:rest foo-zot)
  (check-equal? (foo1 1 2 3 4) (foo 1 #:zot (list 2 3 4)))
  )
