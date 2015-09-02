How can I make `struct-copy` work with an enhanced constructor?

    (struct x (y) #:transparent)
	(define-struct-defaults X x ([y 123]))
	(struct-copy x (X) (y 99)) ;; ok
	(struct-copy X (X) (y 99)) ;; fails because not syntax-bound properly

Introduce rest-list binding, e.g.

    (struct node (id children) #:transparent)
    (define-struct-defaults >node node (#:id [node-id #f]) #:rest children)

Probably best to not support optional positional args and rest-lists
at the same time.
