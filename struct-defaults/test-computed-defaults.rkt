#lang racket/base

(module+ test
  (require rackunit)
  (require "main.rkt")

  (struct x (w z y) #:prefab) ;; N.B. z precedes y! There was a bug
                              ;; here where if z preceded y in the
                              ;; struct definition, but the default
                              ;; initializer for z *followed* y in the
                              ;; define-struct-defaults, it would bind
                              ;; the wrong value.

  (define-struct-defaults make-x x (#:y [x-y 123]
                                    #:z [x-z (case x-y
                                               [(123) 234]
                                               [else 345])]))

  (check-equal? (make-x 0) (x 0 234 123))
  (check-equal? (make-x 0 #:y 99) (x 0 345 99))

  (define-struct-defaults make-x2 x (#:y [x-y 123]
                                     #:z [x-z (case x-y
                                                [(123) 234]
                                                [else 345])])
    #:rest x-w)

  (check-equal? (make-x2 0 1 2) (x (list 0 1 2) 234 123))
  (check-equal? (make-x2 0 1 2 #:y 99) (x (list 0 1 2) 345 99)))
