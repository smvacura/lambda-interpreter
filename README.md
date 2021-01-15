# lambda interpreter

A REPL for a toy functional programming language based on anonymous functions and expressions.

***

## Syntax:

The language grammar is as follows:

    e ::= nat
        | bool
        | id
        | e bop e
        | if e then e else e
        | \e.e
        | e e
    
    nat ::= <naturals>
    
    bool ::= true | false
    
    id ::= <letters>
    
    bop ::= +
          | -
          | *
          | /
          | and
          | or
          | =
          | !=
          | >
          | <
        
Variable binding is done through lambda-expressions and application: `\x.e y` binds the result of evaluating `y` to identifier `x`. 
This is equivalent to `let x = y in e` in `F#` and `OCaml`. Lambda application is nested; `\x.\y.e a b` is equivalent to `\x.(\y.e a) b`, so
application is done in _reverse order_. This is done because any expression can be the right-hand-side of an application, so `(\x.\y.e) (a b)`
would represent the application of expression `(a b)` (itself an application) to the lambda `\y.e`. This also means that every lambda is
curried. `\x.\y.e a` is equivalent to `\x.e[y:=a]`.

***

## Types
This language is _strongly typed_, meaning type conversion is not possible and attempting to use a binary operator with the wrong type
will result in a type error. Because this is an interpreted language, it is dynamically typed. Coming soon is a type-inference algorithm that
will infer types of curried and unsaturated lambdas.

***

## Future features
* Grammar extension (possibly allowing persistent binding of unsaturated lambdas)
* Type inference
* External functions (succ, etc)

***

## Current Bugs
* Nested lambdas in non-lambda expressions (eg `\x.if \y.e a then e else e`) don't fully evaluate
