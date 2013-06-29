# Mini-Core
## Overview
Mini-Core is an implementation of the Core language described in [Implementing Functional Languages: A Tutorial](http://research.microsoft.com/en-us/um/people/simonpj/papers/pj-lester-book/) by Simon Peyton Jones & David Lester. I'm following the book fairly closely, but I'm using Haskell instead of Miranda. I've diverged slightly by adding some concrete syntax for specifying data constructors and matching on them in case-expressions.

## Usage
mini-core compiles a file to G-code and executes it in a virtual G-Machine.
    Usage: mini-core [OPTION...] file
      -v  --verbose      Print each machine state as program executes
      -s  --step         Single-step through program execution (enter -> next, q -> quit)
      -p  --prettyprint  Just show program after parsing
      -t  --transform    Just show program after constructor generation and lambda lifting
      -h  --help         Print usage and exit

## Example Program
A program is just a sequence of supercombinators. Execution proceeds by reducing the supercombinator `main`. Simple algebraic data types are supported using tagged constructors.

```haskell
-- A List data specification; specifies the shape of our constructors
data List = Cons x xs | Nil;

-- mini-core is non-strict; we can construct infinite data structures
infinite x = Cons x (infinite (x + 1));

-- Case expressions make a multi-way branch based on the scrutinee's
-- tag and bind its components to the names preceding the arrow
take n ls = if (n <= 0)
                Nil
                (case ls of {
                    Cons x xs -> Cons x (take (n - 1) xs);
                    Nil       -> Nil;
                });

map f ls = case ls of {
    Cons x xs -> Cons (f x) (map f xs);
    Nil       -> Nil;
};

-- Print the squares of the first 10 elements of an infinite list
-- (Now with lambdas!)
main = map (\x -> x * x) (take 10 (infinite 0))
```

## Warning
At the moment, mini-core is practically untyped, so it will let you attempt to do <i>silly things</i>.

