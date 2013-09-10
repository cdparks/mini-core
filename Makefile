test: mini-core
	bin/mini-core examples/arithmetic.core  | diff tests/arithmetic.out -
	bin/mini-core examples/factorial.core   | diff tests/factorial.out -
	bin/mini-core examples/fibonacci.core   | diff tests/fibonacci.out -
	bin/mini-core examples/infinite.core    | diff tests/infinite.out -
	bin/mini-core examples/fixedpoint.core  | diff tests/fixedpoint.out -
	bin/mini-core examples/map.core         | diff tests/map.out -
	bin/mini-core examples/fold.core        | diff tests/fold.out -
	bin/mini-core examples/freevar.core     | diff tests/freevar.out -
	bin/mini-core examples/polymorphic.core | diff tests/polymorphic.out -

mini-core: src/*.hs
	cabal configure
	cabal build
	mkdir -p bin
	cp dist/build/mini-core/mini-core bin/mini-core

update: mini-core
	bin/mini-core examples/arithmetic.core  > tests/arithmetic.out
	bin/mini-core examples/factorial.core   > tests/factorial.out
	bin/mini-core examples/fibonacci.core   > tests/fibonacci.out
	bin/mini-core examples/infinite.core    > tests/infinite.out
	bin/mini-core examples/fixedpoint.core  > tests/fixedpoint.out
	bin/mini-core examples/map.core         > tests/map.out
	bin/mini-core examples/fold.core        > tests/fold.out
	bin/mini-core examples/freevar.core     > tests/freevar.out
	bin/mini-core examples/polymorphic.core > tests/polymorphic.out -

loud: mini-core
	bin/mini-core examples/arithmetic.core
	bin/mini-core examples/factorial.core
	bin/mini-core examples/fibonacci.core
	bin/mini-core examples/infinite.core
	bin/mini-core examples/fixedpoint.core
	bin/mini-core examples/map.core
	bin/mini-core examples/fold.core
	bin/mini-core examples/freevar.core
	bin/mini-core examples/polymorphic.core

pretty: mini-core
	bin/mini-core --show-parse examples/arithmetic.core
	bin/mini-core --show-parse examples/factorial.core
	bin/mini-core --show-parse examples/fibonacci.core
	bin/mini-core --show-parse examples/infinite.core
	bin/mini-core --show-parse examples/fixedpoint.core
	bin/mini-core --show-parse examples/map.core
	bin/mini-core --show-parse examples/fold.core
	bin/mini-core --show-parse examples/freevar.core
	bin/mini-core --show-parse examples/polymorphic.core

typecheck: mini-core
	bin/mini-core --show-types examples/arithmetic.core
	bin/mini-core --show-types examples/factorial.core
	bin/mini-core --show-types examples/fibonacci.core
	bin/mini-core --show-types examples/infinite.core
	bin/mini-core --show-types examples/fixedpoint.core
	bin/mini-core --show-types examples/map.core
	bin/mini-core --show-types examples/fold.core
	bin/mini-core --show-types examples/freevar.core
	bin/mini-core --show-types examples/polymorphic.core

transform: mini-core
	bin/mini-core --show-simple examples/arithmetic.core
	bin/mini-core --show-simple examples/factorial.core
	bin/mini-core --show-simple examples/fibonacci.core
	bin/mini-core --show-simple examples/infinite.core
	bin/mini-core --show-simple examples/fixedpoint.core
	bin/mini-core --show-simple examples/map.core
	bin/mini-core --show-simple examples/fold.core
	bin/mini-core --show-simple examples/freevar.core
	bin/mini-core --show-simple examples/polymorphic.core

clean:
	cabal clean
	rm -f bin/mini-core

