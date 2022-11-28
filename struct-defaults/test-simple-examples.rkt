#lang racket/base

(module+ test
  (require rackunit)
  (require racket/match)
  (require "main.rkt")

  (let ()
    (struct person (name age extra) #:prefab)
    (define-struct-defaults person/defaults person ([person-age 'unknown] [person-extra '()]))

    (check-equal? (person "Alice" 'unknown '()) (person/defaults "Alice"))
    (check-equal? (person "Alice" 170 '()) (person/defaults "Alice" 170))
    (check-equal? (person "Alice" 170 '(a b c)) (person/defaults "Alice" 170 '(a b c)))
    (check-exn #px"person/defaults: arity mismatch" (lambda () (person/defaults)))
    ;; (check-exn #px"Missing pattern for field person-name"
    ;;            (lambda ()
    ;;              (match (person "Alice" 'unknown '()) [(person/defaults) #t])))
    (check-equal? #t (match (person "Alice" 'unknown '()) [(person/defaults "Alice") #t] [_ #f]))
    (check-equal? #f (match (person "Alice" 'unknown '()) [(person/defaults "NotAlice") #t] [_ #f]))
    (check-equal? #t (match (person "Alice" 170 '(a b c)) [(person/defaults "Alice") #t] [_ #f]))
    (check-equal? #t (match (person "Alice" 170 '(a b c)) [(person/defaults "Alice" 170) #t] [_ #f]))
    (check-equal? #f (match (person "Alice" 170 '(a b c)) [(person/defaults "Alice" 169) #t] [_ #f]))
    (check-equal? #t (match (person "Alice" 170 '(a b c)) [(person/defaults "Alice" 170 '(a b c)) #t] [_ #f]))
    (check-equal? #f (match (person "Alice" 170 '(a b c)) [(person/defaults "Alice" 170 '(a b d)) #t] [_ #f]))
    )

  (let ()
    (struct person (name age extra) #:prefab)
    (define-struct-defaults person/defaults person ([person-age 'unknown]))

    (check-equal? (person "Alice" 'unknown '()) (person/defaults "Alice" '()))
    (check-equal? (person "Alice" 170 '(a b c)) (person/defaults "Alice" '(a b c) 170))
    (check-exn #px"person/defaults: arity mismatch" (lambda () (person/defaults)))
    (check-exn #px"person/defaults: arity mismatch" (lambda () (person/defaults "Alice")))
    (check-equal? #t (match (person "Alice" 170 '(a b c)) [(person/defaults "Alice" '(a b c)) #t] [_ #f]))
    (check-equal? #t (match (person "Alice" 170 '(a b c)) [(person/defaults "Alice" '(a b c) 170) #t] [_ #f]))
    (check-equal? #f (match (person "Alice" 170 '(a b c)) [(person/defaults "Alice" '(a b c) 169) #t] [_ #f]))
    )

  (let ()
    (struct person (name age extra) #:prefab)
    (define-struct-defaults person/defaults person (#:age [person-age 'unknown]) #:keywords-only)

    (check-equal? (person "Alice" 'unknown '()) (person/defaults #:name "Alice" #:extra '()))
    (check-equal? (person "Alice" 170 '(a b c)) (person/defaults #:name "Alice" #:extra '(a b c) #:age 170))
    (check-exn #px"arity mismatch" (lambda () (person/defaults)))
    (check-exn #px"person/defaults: arity mismatch" (lambda () (person/defaults "Alice")))
    (check-exn #px"required keyword argument not supplied" (lambda () (person/defaults #:name "Alice")))
    (check-equal? #t (match (person "Alice" 170 '(a b c)) [(person/defaults #:name "Alice" #:extra '(a b c)) #t] [_ #f]))
    (check-equal? #t (match (person "Alice" 170 '(a b c)) [(person/defaults #:name "Alice" #:extra '(a b c) #:age 170) #t] [_ #f]))
    (check-equal? #f (match (person "Alice" 170 '(a b c)) [(person/defaults #:name "Alice" #:extra '(a b c) #:age 169) #t] [_ #f]))
    )

  (let ()
    (struct person (name age extra) #:prefab)
    (define-struct-defaults person/defaults person ([person-age 'unknown]) #:rest person-extra)

    (check-equal? (person "Alice" 'unknown '()) (person/defaults "Alice"))
    (check-equal? (person "Alice" 170 '()) (person/defaults "Alice" 170))
    (check-equal? (person "Alice" 170 '(a b c)) (person/defaults "Alice" 170 'a 'b 'c))
    (check-exn #px"person/defaults: arity mismatch" (lambda () (person/defaults)))
    (check-equal? #t (match (person "Alice" 170 '()) [(person/defaults "Alice") #t] [_ #f]))
    (check-equal? #t (match (person "Alice" 170 '()) [(person/defaults "Alice" 170) #t] [_ #f]))
    (check-equal? #f (match (person "Alice" 170 '()) [(person/defaults "Alice" 169) #t] [_ #f]))
    (check-equal? #f (match (person "Alice" 170 '(a b c)) [(person/defaults "Alice" 170) #t] [_ #f]))
    (check-equal? #t (match (person "Alice" 170 '(a b c)) [(person/defaults "Alice" 170 'a 'b 'c) #t] [_ #f]))
    )

  (let ()
    (struct person (name age extra) #:prefab)
    (define-struct-defaults person/defaults person (#:age [person-age 'unknown]) #:rest person-extra)

    (check-equal? (person "Alice" 'unknown '()) (person/defaults "Alice"))
    (check-equal? (person "Alice" 170 '()) (person/defaults "Alice" #:age 170))
    (check-equal? (person "Alice" 170 '(a b c)) (person/defaults "Alice" #:age 170 'a 'b 'c))
    (check-exn #px"person/defaults: arity mismatch" (lambda () (person/defaults)))
    (check-equal? #t (match (person "Alice" 170 '()) [(person/defaults "Alice") #t] [_ #f]))
    (check-equal? #t (match (person "Alice" 170 '()) [(person/defaults "Alice" #:age 170) #t] [_ #f]))
    (check-equal? #f (match (person "Alice" 170 '()) [(person/defaults "Alice" #:age 169) #t] [_ #f]))
    (check-equal? #f (match (person "Alice" 170 '(a b c)) [(person/defaults "Alice" #:age 170) #t] [_ #f]))
    (check-equal? #t (match (person "Alice" 170 '(a b c)) [(person/defaults "Alice" #:age 170 _ ...) #t] [_ #f]))
    (check-equal? #t (match (person "Alice" 170 '(a b c)) [(person/defaults "Alice" #:age 170 'a 'b 'c) #t] [_ #f]))
    )

  (let ()
    (struct person (first-name
                    last-name
                    nickname
                    age
                    fictional?
                    extra)
      #:prefab)

    (define-struct-defaults person1 person
      ([person-age 'unknown]
       #:nickname [person-nickname 'none]
       #:fictional person-fictional?)
      #:rest person-extra)

    (check-equal? (person "Alice" "Liddell" 'none 170 #t '(a b c))
                  (person1 #:fictional #t "Alice" "Liddell" 170 'a 'b 'c))
    (check-equal? (person "Alice" "Liddell" "Alice" 170 #t '(a b c))
                  (person1 #:fictional #t "Alice" "Liddell" #:nickname "Alice" 170 'a 'b 'c))
    (check-equal? (person "Alice" "Liddell" 'none 'unknown #t '())
                  (person1 #:fictional #t "Alice" "Liddell")))

  )
