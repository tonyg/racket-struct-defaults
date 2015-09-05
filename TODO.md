How can I make `struct-copy` work with an enhanced constructor?

    (struct x (y) #:transparent)
	(define-struct-defaults X x ([y 123]))
	(struct-copy x (X) (y 99)) ;; ok
	(struct-copy X (X) (y 99)) ;; fails because not syntax-bound properly
