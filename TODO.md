How can I make `struct-copy` work with an enhanced constructor?

    (struct x (y) #:transparent)
	(define-struct-defaults X x ([y 123]))
	(struct-copy x (X) (y 99)) ;; ok
	(struct-copy X (X) (y 99)) ;; fails because not syntax-bound properly

Could the macro be enhanced to allow a default for (struct x (y z)) of x-z with the value
supplied for x-y? Like, initializing person-nickname with person-first-name's value.
