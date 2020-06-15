---
title: Functional Programming
layout: notes
---

list=refl
functor=lift a function into a context, eg list
applicative=same but the function can have any number of args
monad=functor but you're allowed to squish it when nested. Use it to chain exprs with functor results
comonad=you don't need it
traversable=the FP cheat code


# Overview
While LISP by McCarthy in 1958 was one of the first functional programming languages, it all started back with [Alonzo Church](https://en.wikipedia.org/wiki/Alonzo_Church) in the 1930s who wanted to prove the foundations of mathematics.  His creation of lambda calculus (also written as λ-calculus) is a formal system in mathematical logic for expressing computation based on function abstraction and application using variable binding and substitution.

Any program that can be authored can be done so using these three constructs:

```
exp -> var                         - variables
           | λvar. exp)            - abstraction
           | (exp1, exp2)       - applicatiopn
```


# LISP 

```
(lambda (arg) (+ arg 1))

(defun factorial (n)
      (if (= n 0) 1
      (* n (factorial (-n 1)))))

;; recursive
(defun factorial (n &optional (acc 1))
             (if (=n 0) acc
                   (factorial (- n 1) (* acc n))))
```

* [Lambdas](lambda)
* [Recursion](recursion)
* [Tree Structures](tree-structures)
* [Garbage Collection](garbage-collection)
* [Meta programming](meta-programming)
* [Read-eval-print loop](repl)
* [Metacircular interpreter](metacircular-interpreter)
* [Higher-order module system](higher-order-module-system)

# Scheme (1975)

* [Lexical scoping](lexical-scoping)
* [Tail call optimisation](tail-call-optimisation)
* [First-class continuations](first-class-continuations)

# Racket (1994)
* [Domain-specific languages](domain-specific-languages)

# Erlang (1986)
* Concurrent
* Distributed
* Fault-tolerant
* Hot-swapping

# Elixir (2011)
* [Meta programming](meta-programming)
* [Protocols](protocols)

# Standard ML (1973)
Developed by a group (Miller/Tofte/Harper) that were interested in theorem proving and ways to automate their proofs. 

```ml
fun fact (0 : int) : int = 1
     |  fact (n: int) : int = n * fact (n - 1)
```
* [Algebraic data types](algebraic-data-types)
* [Pattern matching](pattern-matching)
* [Parametric polymorphism](generics)
* [Type inference](type-inference)

# OCaml (1996)
* Object System
* Structural subtyping

# Miranda (1985)

```
fact :: num -> num || optional
fact 0 = 1
fact (n+1) = (n+1) * fact n

primes = sieve [2..]
sieve (p:x) = p : sieve [n |n <- x ; n mod [ ~= 0]
```
* Lazy
* List comprehensions

# Haskell (1990)

```
fact :: Num a => a -> a	-- type signature is optional
fact 0 =n 1
factn = n * fact (n-1)
```

* [Type-classes](type-classes)
* [Monads](category-theory#monads)
* [Type families](type-families)

# Agda (1999)
* [Dependent Types](dependent-types.md)

# Idris (2011)


# Teaching Functional Programming
* https://byorgey.wordpress.com/2018/05/06/conversations-with-a-six-year-old-on-functional-programming/
