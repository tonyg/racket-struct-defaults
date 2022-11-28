# raco pkg install struct-defaults

Macros for flexible provision of (1) default values, (2) named, rather than positional
parameters and (3) rest-argument support for [Racket
struct](https://docs.racket-lang.org/racket-glossary/index.html#%28part._.Struct%29)
constructors and patterns.

## Macro: `define-struct-defaults`

(`define-struct-defaults` *new-ctor* *struct-type-id* (*spec* ...)  
  «`#:rest` *rest-id*»  
  «`#:keywords-only`»)

where

 - *new-ctor* is an identifier
 - *struct-type-id* is an identifier that should refer to a `struct` definition
 - *spec* = [*accessor* *value*] | `#:`*kw* [*accessor* *value*] | `#:`*kw* *accessor*
 - *rest-id*, if supplied, names the struct field to receive any supplied rest-arguments
 - if `#:keywords-only` is present, positional arguments are disallowed and keyword argument
   names are inferred where not explicitly given

Binds *new-ctor* to a [match
expander](https://docs.racket-lang.org/reference/match.html#%28part._.Extending_match%29),
which can be used in both expression and pattern-match context.

In an expression, *new-ctor* evaluates to an enhanced constructor procedure with support for
default and keyword arguments:

 - Mandatory fields (those not mentioned in any *spec*) are expected first.
 - After the mandatory fields, zero or more of the positional default fields (those with
   *spec*s of the form [*accessor* *value*]) are expected.
 - If all the positional default fields have corresponding arguments and a *rest-id* field was
   specified, any remaining arguments contribute to it. Otherwise, extra arguments are an
   error.
 - Keyword-defaulted fields (*spec*s of form `#:`*kw* [*accessor* *value*]) and mandatory
   keyword fields (*spec*s of form `#:`*kw* *accessor*) may be placed anywhere.

In a pattern, *new-ctor* expects a similar arrangement of subpatterns:

 - Mandatory field subpatterns are expected first.
 - Zero or more of the positional default field subpatterns may then follow. If omitted, a
   **wildcard** pattern is used for the omitted subpattern.
 - If all the positional default fields have corresponding subpatterns, subsequent subpatterns
   contribute to a (`list` ...) subpattern for the *rest-id* field, if one was supplied.
   Otherwise, extra subpatterns are an error.
 - Keyword-defaulted fields and mandatory keyword fields may have subpatterns placed anywhere.

If *rest-id* is supplied, it must denote a field in *struct-type-id* and the field it denotes
must not have a default value.

### Examples

Given the following definitions:

```racket
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
```

the following are possible REPL interactions:

```racket
> (person1 #:fictional #t "Alice" "Liddell" 170 'a 'b 'c)
'#s(person "Alice" "Liddell" 170 none #t (a b c))

> (person1 #:fictional #t "Alice" "Liddell" #:nickname "Alice" 170 'a 'b 'c)
'#s(person "Alice" "Liddell" 170 "Alice" #t (a b c))

> (person1 #:fictional #t "Alice" "Liddell")
'#s(person "Alice" "Liddell" unknown none #t ())
```

Note that the `#:fictional` keyword argument is mandatory:

```racket
> (person1 "Alice" "Liddell")
; raise-missing-kw: arity mismatch;
;  the expected number of arguments does not match the given number
;   expected: 3
;   given: 2
```

... as is the `last-name` positional argument:

```racket
> (person1 #:fictional #t "Alice")
; application: no case matching 1 non-keyword argument
;   procedure: person1
;   arguments...:
;    "Alice"
;    #:fictional #t
```

The following form defines a constructor/match-expander `person2` that only accepts keyword
arguments:

```racket
(define-struct-defaults person2 person () #:keywords-only)
```

This can be useful for structs that have a large number of fields. Some REPL interactions:

```racket
> (person2 #:first-name "Alice"
           #:last-name "Liddell"
           #:nickname 'none
           #:age 170
           #:fictional? #t
           #:extra '())
'#s(person "Alice" "Liddell" none 170 #t ())

> (person2 #:first-name "Alice")
; application: required keyword argument not supplied
;   procedure: person2
;   required keyword: #:age
; [,bt for context]

> (person2)
; raise-missing-kw: arity mismatch;
;  the expected number of arguments does not match the given number
;   expected: 3
;   given: 2
; [,bt for context]
```

Using `#:keywords-only` with a positional default is not allowed:

```racket
> (define-struct-defaults person3 person ([person-age 'unknown]) #:keywords-only)
; person3: Invalid default specifications: #<syntax:main.rkt:116:31
;   ((person-age (quote unknown)))> [,bt for context]
```

However, keyword defaults are fine:

```racket
(define-struct-defaults person4 person (#:age [person-age 'unknown]) #:keywords-only)

> (person4 #:first-name "Alice"
           #:last-name "Liddell"
           #:nickname 'none
           #:fictional? #t
           #:extra '())
'#s(person "Alice" "Liddell" none unknown #t ())
```

And explicitly-given keywords do not have to match the keywords inferred from the accessor
names:

```racket
(define-struct-defaults person5 person (#:nick person-nickname
                                        #:years [person-age 'unknown])
  #:keywords-only)

> (person5 #:first-name "Alice"
           #:last-name "Liddell"
           #:nick "Alice"
           #:years 170
           #:fictional? #t
           #:extra '())
'#s(person "Alice" "Liddell" "Alice" 170 #t ())
```

### Inheritance

### Errors

**Cannot retrieve struct-info information for identifier —**.  

**Unexpected keyword**.  

**Missing pattern for field —**.  

**Too few positional arguments**.  

**Cannot have default value for rest argument**.  

**Rest identifier not a field of given struct type**.  

**Partially-opaque struct types not supported**.  

**Unknown fields: —**.  

**Missing keyword pattern —**.  

**Invalid define-struct-defaults flags: —**.  

**Invalid default specifications: —**.  
