#lang racket/base
;; Souped-up structs.

(provide define-struct-defaults
         struct-out/defaults)

(require racket/match)
(require racket/provide-syntax)
(require (for-syntax racket/base
                     racket/struct-info
                     (only-in racket/list
                              append-map
                              filter-map
                              filter-not
                              drop
                              remf)))

(begin-for-syntax
  (define (info struct-type-id)
    (apply values
           (extract-struct-info
            (syntax-local-value
             struct-type-id
             (lambda ()
               (raise-syntax-error
                'info
                (format "Cannot retrieve struct-info information for identifier ~v"
                        struct-type-id)))))))

  (define (find-keyworded-pattern ctor-id sought-kw pats)
    (let loop ((pats pats))
      (syntax-case pats ()
        [() #'_]
        [(kw pat rest ...)
         (keyword? (syntax->datum #'kw))
         (if (equal? (syntax->datum #'kw) sought-kw)
             #'pat
             (loop #'(rest ...)))]
        [(pat rest ...)
         (loop #'(rest ...))])))

  (define (extract-positional-patterns ctor-id pats)
    (let loop ((pats pats))
      (syntax-case pats ()
        [() '()]
        [(kw pat rest ...)
         (keyword? (syntax->datum #'kw))
         (loop #'(rest ...))]
        [(pat rest ...)
         (if (keyword? (syntax->datum #'pat))
             (raise-syntax-error ctor-id "Unexpected keyword")
             (cons #'pat (loop #'(rest ...))))])))

  (define (find-positional-pattern ctor-id name index pats mandatory?)
    (define positionals (extract-positional-patterns ctor-id pats))
    (cond
      [(> (length positionals) index) (list-ref positionals index)]
      [(not mandatory?) #'_]
      [else (raise-syntax-error ctor-id (format "Missing pattern for field ~a" name))]))

  (define (find-rest-pattern ctor-id mandatory-count positional-count pats)
    (define positionals (extract-positional-patterns ctor-id pats))
    (cond [(>= (length positionals) (+ mandatory-count positional-count))
           #`(list #,@(drop positionals (+ mandatory-count positional-count)))]
          [(>= (length positionals) mandatory-count)
           #`(list)]
          [else
           (raise-syntax-error ctor-id "Too few positional arguments")])))

(define-syntax define-struct-defaults
  (lambda (stx)
    (syntax-case stx ()
      [(_ new-ctor struct-type-id (spec ...))
       #'(define-struct-defaults new-ctor struct-type-id (spec ...) #:rest #f)]
      [(_ new-ctor struct-type-id (spec ...) #:rest rest-id)
       (let ()

         (define defaults
           (let walk ((specs #'(spec ...)))
             (syntax-case specs ()
               [() '()]
               [([accessor value] rest ...)
                (identifier? #'accessor)
                (cons (list #'accessor #f #'value) (walk #'(rest ...)))]
               [(kw [accessor value] rest ...)
                (and (keyword? (syntax->datum #'kw))
                     (identifier? #'accessor))
                (cons (list #'accessor (syntax->datum #'kw) #'value) (walk #'(rest ...)))]
               [(kw accessor rest ...)
                (and (keyword? (syntax->datum #'kw))
                     (identifier? #'accessor))
                (cons (list #'accessor (syntax->datum #'kw)) (walk #'(rest ...)))])))

         (define ((id=? i) j) (free-identifier=? i j))

         (define (lookup-default accessor-id)
           (assf (id=? accessor-id) defaults))

         (define (binders-in-default-order ids)
           (append-map entry->binder
                       (filter (lambda (entry) (findf (id=? (car entry)) ids))
                               defaults)))

         (define (entry->binder entry)
           (if (cadr entry) ;; keyword argument provided
               (if (null? (cddr entry)) ;; no default value provided
                   (list (cadr entry) (car entry))
                   (list (cadr entry) (list (car entry) (caddr entry))))
               (list (list (car entry) (caddr entry)))))

         (define-values (_type-id ctor-id _pred-id accessor-ids-rev _mutator-ids-rev super-type-id)
           (info #'struct-type-id))
         (define accessor-ids (reverse accessor-ids-rev))

         (define positional-default-ids
           (filter-map (lambda (entry)
                         (and (not (cadr entry))
                              (findf (id=? (car entry)) accessor-ids)))
                       defaults))

         (define have-rest-id? (syntax->datum #'rest-id))

         (when have-rest-id?
           (when (lookup-default #'rest-id)
             (raise-syntax-error #f "Cannot have default value for rest argument" stx))
           (when (not (memf (id=? #'rest-id) accessor-ids))
             (raise-syntax-error #f "Rest identifier not a field of given struct type" stx)))

         (define positional-accessor-ids
           (let ((ids (append (filter-not lookup-default accessor-ids)
                              positional-default-ids)))
             (if have-rest-id?
                 (remf (id=? #'rest-id) ids)
                 ids)))

         (define (positional-index-of accessor-id)
           (let walk ((index 0) (ids positional-accessor-ids))
             (if (free-identifier=? accessor-id (car ids))
                 index
                 (walk (+ index 1) (cdr ids)))))

         (define (extract-pattern accessor-id pats-stx)
           (define entry (lookup-default accessor-id))
           (cond
             [(and have-rest-id? (free-identifier=? accessor-id #'rest-id))
              #`(find-rest-pattern 'new-ctor
                                   #,(- (length accessor-ids)
                                        (length defaults)
                                        1)
                                   #,(length positional-default-ids)
                                   #,pats-stx)]
             [(and entry (cadr entry))
              #`(find-keyworded-pattern 'new-ctor '#,(cadr entry) #,pats-stx)]
             [else
              #`(find-positional-pattern 'new-ctor
                                         '#,accessor-id
                                         #,(positional-index-of accessor-id)
                                         #,pats-stx
                                         #,(not entry))]))

         (when (and (pair? accessor-ids)
                    (eq? (car accessor-ids) #f))
           (raise-syntax-error #f "Partially-opaque struct types not supported" stx))

         (define unknown-defaults
           (filter (lambda (entry) (not (memf (id=? (car entry)) accessor-ids))) defaults))
         (when (not (null? unknown-defaults))
           (raise-syntax-error #f (format "Unknown fields: ~v" unknown-defaults)))

         #`(begin
             (define ctor-proc
               (procedure-rename
                #,(if have-rest-id?
                      (let ((remaining-ids (filter-not (id=? #'rest-id) accessor-ids))
                            ;; v TODO: If I say #'rest-id for
                            ;; rest-accessor below, it doesn't work
                            ;; properly. Why not? See test in
                            ;; test-provide.rkt.
                            (rest-accessor (findf (id=? #'rest-id) accessor-ids)))
                        #`(lambda (#,@(filter-not lookup-default remaining-ids)
                                   #,@(binders-in-default-order remaining-ids)
                                   . #,rest-accessor)
                            (#,ctor-id #,@accessor-ids)))
                      #`(lambda (#,@(filter-not lookup-default accessor-ids)
                                 #,@(binders-in-default-order accessor-ids))
                          (#,ctor-id #,@accessor-ids)))
                'new-ctor))
             (define-match-expander new-ctor
               (lambda (cstx)
                 (syntax-case cstx ()
                   [(_ . pats)
                    #,(with-syntax [[(acc ...) (generate-temporaries accessor-ids)]
                                    [(finder ...) (map (lambda (i) (extract-pattern i #'#'pats))
                                                       accessor-ids)]]
                        #`(with-syntax [[acc finder] ...]
                            #'(#,ctor-id acc ...)))]))
               (lambda (cstx)
                 (syntax-case cstx ()
                   [(_ . args)
                    #'(ctor-proc . args)]
                   [i
                    (identifier? #'i)
                    #'ctor-proc])))))])))

(define-provide-syntax struct-out/defaults
  (lambda (stx)
    (syntax-case stx ()
      [(_ [defaults-id struct-id])
       #'(combine-out (except-out (struct-out struct-id) struct-id)
                      (rename-out [defaults-id struct-id]))])))

(module+ test
  (require rackunit)

  (struct x (y) #:transparent)
  (struct z x () #:transparent)
  (struct q x (q) #:transparent)

  (define-syntax (check-both-directions stx)
    (syntax-case stx ()
      [(_ fancy plain)
       ;; Go round the houses a bit here to preserve line numbers of
       ;; the actual test cases.
       (with-syntax ((fwd (syntax/loc stx (check-equal? fancy plain)))
                     (rev (syntax/loc stx
                            (check-equal? (match plain
                                            [fancy 'ok]
                                            [_ (list 'fail 'fancy 'plain)])
                                          'ok))))
         #'(begin fwd rev))]))

  (define-struct-defaults x1 x ())
  (check-both-directions (x1 99) (x 99))

  (define-struct-defaults x2 x ([x-y 33]))
  (check-both-directions (x2) (x 33))
  (check-both-directions (x2 99) (x 99))

  (define-struct-defaults x3 x (#:y [x-y 33]))
  (check-both-directions (x3) (x 33))
  ;;(x3 99) ;; TODO: racket bug?
  (check-both-directions (x3 #:y 99) (x 99))

  (define-struct-defaults x4 x (#:y x-y))
  (check-both-directions (x4 #:y 99) (x 99))

  (define-struct-defaults z1 z ())
  (check-both-directions (z1 99) (z 99))

  (define-struct-defaults z2 z ([x-y 33]))
  (check-both-directions (z2) (z 33))
  (check-both-directions (z2 99) (z 99))

  (define-struct-defaults z3 z (#:y [x-y 33]))
  (check-both-directions (z3) (z 33))
  (check-both-directions (z3 #:y 99) (z 99))

  (define-struct-defaults z4 z (#:y x-y))
  (check-both-directions (z4 #:y 99) (z 99))

  (define-struct-defaults q1 q ())
  (check-both-directions (q1 99 88) (q 99 88))

  (define-struct-defaults q2 q ([x-y 33]))
  (check-both-directions (q2 88) (q 33 88))
  (check-both-directions (q2 88 99) (q 99 88)) ;; N.B. flipped!

  (define-struct-defaults q3 q ([q-q 44]))
  (check-both-directions (q3 99) (q 99 44))
  (check-both-directions (q3 99 88) (q 99 88))

  (define-struct-defaults q4 q (#:y [x-y 33]))
  (check-both-directions (q4 88) (q 33 88))
  (check-both-directions (q4 88 #:y 99) (q 99 88))

  (define-struct-defaults q5 q (#:q [q-q 44]))
  (check-both-directions (q5 99) (q 99 44))
  (check-both-directions (q5 99 #:q 88) (q 99 88))

  (define-struct-defaults q6 q (#:y x-y))
  (check-both-directions (q6 88 #:y 99) (q 99 88))

  (define-struct-defaults q7 q (#:q q-q))
  (check-both-directions (q7 99 #:q 88) (q 99 88))

  (define-struct-defaults q8 q ([x-y 33] [q-q 44]))
  (check-both-directions (q8) (q 33 44))
  (check-both-directions (q8 99) (q 99 44))
  (check-both-directions (q8 99 88) (q 99 88))

  (define-struct-defaults q9 q ([q-q 44] [x-y 33]))
  (check-both-directions (q9) (q 33 44))
  (check-both-directions (q9 99) (q 33 99))
  (check-both-directions (q9 99 88) (q 88 99))

  (define-struct-defaults q10 q ([x-y 33] #:q [q-q 44]))
  (check-both-directions (q10) (q 33 44))
  (check-both-directions (q10 99) (q 99 44))
  (check-both-directions (q10 #:q 88) (q 33 88))
  (check-both-directions (q10 99 #:q 88) (q 99 88))

  (define-struct-defaults q11 q ([x-y 33] #:q q-q))
  (check-both-directions (q11 #:q 88) (q 33 88))
  (check-both-directions (q11 99 #:q 88) (q 99 88))

  (define-struct-defaults q12 q (#:y [x-y 33] [q-q 44]))
  (check-both-directions (q12) (q 33 44))
  (check-both-directions (q12 88) (q 33 88))
  (check-both-directions (q12 #:y 99) (q 99 44))
  (check-both-directions (q12 88 #:y 99) (q 99 88))

  (define-struct-defaults q13 q (#:y x-y [q-q 44]))
  (check-both-directions (q13 #:y 99) (q 99 44))
  (check-both-directions (q13 88 #:y 99) (q 99 88))

  (define-struct-defaults q14 q (#:y [x-y 33] #:q [q-q 44]))
  (check-both-directions (q14) (q 33 44))
  (check-both-directions (q14 #:y 99) (q 99 44))
  (check-both-directions (q14 #:q 88) (q 33 88))
  (check-both-directions (q14 #:y 99 #:q 88) (q 99 88))

  ;; (syntax->datum (expand-once #'(define-struct-defaults q15 q (#:y x-y #:q q-q))))
  (define-struct-defaults q15 q (#:y x-y #:q q-q))
  (check-both-directions (q15 #:y 99 #:q 88) (q 99 88))

  (define-struct-defaults q16 q (#:y [x-y 123] #:q [q-q x-y]))
  (check-equal? (q16) (q 123 123))

  (define-struct-defaults q17 q (#:y [x-y 123] #:q [q-q (case x-y
                                                          [(123) 234]
                                                          [else 345])]))
  (check-equal? (q17) (q 123 234))
  (check-equal? (q17 #:y 99) (q 99 345))

  (define-struct-defaults q18 q (#:q [q-q x-y] ;; x-y is the global binding at this point
                                 #:y [x-y 123]))
  (check-equal? (q18) (q 123 x-y)) ;; N.B. x-y is a procedure
  (check-equal? (q18 #:y 99) (q 99 x-y)) ;; N.B. x-y is a procedure

  (check-equal? (match (q 99 88)
                  [(q15 #:q (? even? v) #:y _) (list 'ok v)]
                  [_ 'fail])
                (list 'ok 88))

  (struct v q (rest) #:transparent)

  (define-struct-defaults v1 v () #:rest v-rest)
  (check-both-directions (v1 1 2 3 4 5) (v 1 2 '(3 4 5)))
  (check-both-directions (v1 1 2) (v 1 2 '()))
  (check-equal? (match (v 1 2 '(3 4 5)) [(v1 1 2 3 x y) (list x y)]) (list 4 5))

  (define-struct-defaults v2 v (#:q [q-q 'q]) #:rest v-rest)
  (check-both-directions (v2 1 2 3 4 5) (v 1 'q '(2 3 4 5)))
  (check-both-directions (v2 #:q 1 2 3 4 5) (v 2 1 '(3 4 5)))
  (check-both-directions (v2 1 #:q 2 3 4 5) (v 1 2 '(3 4 5)))
  (check-both-directions (v2 1 2 3 #:q 4 5) (v 1 4 '(2 3 5)))

  (define-struct-defaults v3 v (#:q [q-q 'q]) #:rest x-y)
  (check-both-directions (v3 1 2 3 4 5) (v '(2 3 4 5) 'q 1))
  (check-both-directions (v3 #:q 1 2 3 4 5) (v '(3 4 5) 1 2))
  (check-both-directions (v3 1 #:q 2 3 4 5) (v '(3 4 5) 2 1))
  (check-both-directions (v3 1 2 3 #:q 4 5) (v '(2 3 5) 4 1))

  (define-struct-defaults v4 v (#:q q-q) #:rest v-rest)
  (check-both-directions (v4 #:q 1 2 3 4 5) (v 2 1 '(3 4 5)))
  (check-both-directions (v4 1 #:q 2 3 4 5) (v 1 2 '(3 4 5)))
  (check-both-directions (v4 1 2 3 #:q 4 5) (v 1 4 '(2 3 5)))

  (define-struct-defaults v5 v (#:q q-q) #:rest x-y)
  (check-both-directions (v5 #:q 1 2 3 4 5) (v '(3 4 5) 1 2))
  (check-both-directions (v5 1 #:q 2 3 4 5) (v '(3 4 5) 2 1))
  (check-both-directions (v5 1 2 3 #:q 4 5) (v '(2 3 5) 4 1))
  )
