# Mini-Core
## Overview
Mini-Core is an implementation of the Core language described in [Implementing Functional Languages: A Tutorial](http://research.microsoft.com/en-us/um/people/simonpj/papers/pj-lester-book/) by Simon Peyton Jones & David Lester. I'm following the book fairly closely, but I'm using Haskell instead of Miranda. Eventually, I'd like to add some syntactic sugar for better pattern matching and data type specification.

## Usage
mini-core compiles a file to G-code and executes it in a virtual G-Machine.

    Usage: mini-core [OPTION...] file
      -v  --verbose  Print each machine state as program executes
      -s  --step     Single-step through program execution (n -> next, q -> quit)
      -h  --help     Print usage and exit

## Example Program
A program is just a sequence of supercombinators. Execution proceeds by reducing the supercombinator `main`. Simple algebraic data types are supported using tagged constructors.

    -- A List data type
    Cons a b = Pack{3, 2} a b; -- Cons uses the tag 3 and has 2 components
    Nil      = Pack{4, 0};     -- Nil uses the tag 4 and has 0 components

    -- mini-core is non-strict; we can construct inifinite data structures
    infinite x = Cons x (infinite (x + 1));

    -- Case expressions make a multi-way branch based on the scrutinee's
    -- tag and bind its components to the names preceeding the arrow
    take n ls = if (n <= 0)
                    Nil
                    (case ls of
                        <3> x xs -> Cons x (take (n - 1) xs);
                        <4>      -> Nil);

    -- Print the first 10 elements of an inifinite list
    main = take 10 (infinite 0)

## Warning
At the moment, mini-core is practically untyped, so it will let you attempt to do <i>silly things</i>.

