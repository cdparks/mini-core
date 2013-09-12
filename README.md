# Mini-Core
## Overview
Mini-Core started as an implementation of the Core language described in [Implementing Functional Languages: A Tutorial](http://research.microsoft.com/en-us/um/people/simonpj/papers/pj-lester-book/) by Simon Peyton Jones & David Lester. I've diverged slightly by adding let-polymorphic type inference and some concrete syntax for specifying and matching on data constructors.

## Usage
mini-core compiles a file to G-code and executes it in a virtual G-Machine.

    Usage: mini-core [OPTION...] file
      -h  --help         Print usage and exit
          --show-parse   Show program after parsing
          --show-types   Show types after type-checking
          --show-simple  Show program after constructor generation and lambda lifting
          --show-g-code  Show G-code after compilation
          --show-states  Print each machine state as program executes

## Example Program
A program is just a sequence of supercombinators. Execution proceeds by reducing the supercombinator `main`. Simple algebraic data types are supported using tagged constructors.

```haskell
-- A List data specification; specifies the shape of our constructors
data List a = Cons a (List a) | Nil;

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

-- Print the squares of the first 5 elements of an infinite list
-- Like Haskell, we use \ to introduce an anonymous function
main = map (\x -> x * x) (take 5 (infinite 0))
```

Running the compiler on this program produces the following output:

    (Cons 0 (Cons 1 (Cons 4 (Cons 9 (Cons 16 Nil)))))

With `--show-types` we also get the following output:

    ==================== Type Inference ====================
    Cons :: forall a. a -> List a -> List a
    False :: Bool
    Nil :: forall a. List a
    True :: Bool
    infinite :: Int -> List Int
    main :: List Int
    map :: forall a b. (a -> b) -> List a -> List b
    take :: forall a. Int -> List a -> List a

## References
* [Implementing Functional Languages: A Tutorial](http://research.microsoft.com/en-us/um/people/simonpj/papers/pj-lester-book/) by Simon Peyton Jones & David Lester
* [The Implementation of Functional Programming Languages](http://research.microsoft.com/en-us/um/people/simonpj/papers/slpj-book-1987/index.htm) by Simon Peyton Jones
* [Algorithm W Step By Step](http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.65.7733) by Martin Grabmuller
* [Typing Haskell in Haskell](http://web.cecs.pdx.edu/~mpj/thih/) by Mark P. Jones

