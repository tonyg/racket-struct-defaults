#lang racket/base
;; Souped-up structs.

(provide define-struct-defaults
         struct-out/defaults)

(require racket/match)
(require racket/provide-syntax)
(require (for-syntax racket/base
                     racket/struct-info
                     (only-in racket/list append-map filter-not)))

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

  (define (find-keyworded-pattern ctor-id sought-kw pats mandatory?)
    (let loop ((pats pats))
      (syntax-case pats ()
        [()
         (if mandatory?
             (raise-syntax-error ctor-id
                                 (format "Missing mandatory keyword pattern: ~v" sought-kw))
             #'_)]
        [(kw pat rest ...)
         (keyword? (syntax->datum #'kw))
         (if (equal? (syntax->datum #'kw) sought-kw)
             #'pat
             (loop #'(rest ...)))]
        [(pat rest ...)
         (loop #'(rest ...))])))

  (define (find-positional-pattern ctor-id name index pats mandatory?)
    (let loop ((index index) (pats pats))
      (syntax-case pats ()
        [()
         (if mandatory?
             (raise-syntax-error ctor-id
                                 (format "Missing pattern for field ~a" name))
             #'_)]
        [(kw pat rest ...)
         (keyword? (syntax->datum #'kw))
         (loop index #'(rest ...))]
        [(pat rest ...)
         (cond
           [(keyword? (syntax->datum #'kw))
            (raise-syntax-error ctor-id "Unexpected keyword")]
           [(zero? index)
            #'pat]
           [else
            (loop (- index 1) #'(rest ...))])]))))

(define-syntax define-struct-defaults
  (lambda (stx)
    (syntax-case stx ()
      [(_ new-ctor struct-type-id (spec ...))
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

         (define (lookup-default accessor-id)
           (assf (lambda (i) (free-identifier=? i accessor-id)) defaults))

         (define (compute-binder accessor-id)
           (define entry (lookup-default accessor-id))
           (if (cadr entry) ;; keyword argument provided
               (if (null? (cddr entry)) ;; no default value provided
                   (list (cadr entry) accessor-id)
                   (list (cadr entry) (list accessor-id (caddr entry))))
               (list (list accessor-id (caddr entry)))))

         (define-values (_type-id ctor-id _pred-id accessor-ids-rev _mutator-ids-rev super-type-id)
           (info #'struct-type-id))
         (define accessor-ids (reverse accessor-ids-rev))

         (define positional-accessor-ids
           (append (filter-not lookup-default accessor-ids)
                   (filter (lambda (i)
                             (define entry (lookup-default i))
                             (and entry (not (cadr entry))))
                           accessor-ids)))

         (define (positional-index-of accessor-id)
           (let walk ((index 0) (ids positional-accessor-ids))
             (if (free-identifier=? accessor-id (car ids))
                 index
                 (walk (+ index 1) (cdr ids)))))

         (define (extract-pattern accessor-id pats-stx)
           (define entry (lookup-default accessor-id))
           (if (and entry (cadr entry))
               #`(find-keyworded-pattern 'new-ctor '#,(cadr entry) #,pats-stx #,(null? (cddr entry)))
               #`(find-positional-pattern 'new-ctor
                                          '#,accessor-id
                                          #,(positional-index-of accessor-id)
                                          #,pats-stx
                                          #,(not entry))))

         (when (and (pair? accessor-ids)
                    (eq? (car accessor-ids) #f))
           (raise-syntax-error #f "Partially-opaque struct types not supported" stx))

         (define unknown-defaults
           (filter (lambda (entry)
                     (not (memf (lambda (i) (free-identifier=? i (car entry))) accessor-ids)))
                   defaults))
         (when (not (null? unknown-defaults))
           (raise-syntax-error #f (format "Unknown fields: ~v" unknown-defaults)))

         #`(begin
             (define ctor-proc
               (procedure-rename
                (lambda (#,@(filter-not lookup-default accessor-ids)
                         #,@(append-map compute-binder (filter lookup-default accessor-ids)))
                  (#,ctor-id #,@accessor-ids))
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

  (define-syntax-rule (check-both-directions fancy plain)
    (begin
      (check-equal? fancy plain)
      (check-equal? (match plain [fancy 'ok] [_ (list 'fail 'fancy 'plain)]) 'ok)))

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
  (check-both-directions (q9 99) (q 99 44))
  (check-both-directions (q9 99 88) (q 99 88))

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

  (check-equal? (match (q 99 88)
                  [(q15 #:q (? even? v) #:y _) (list 'ok v)]
                  [_ 'fail])
                (list 'ok 88))
  )
