test: mini-core
	bin/mini-core examples/arithmetic.core | diff tests/arithmetic.out -
	bin/mini-core examples/factorial.core  | diff tests/factorial.out -
	bin/mini-core examples/fibonacci.core  | diff tests/fibonacci.out -
	bin/mini-core examples/infinite.core   | diff tests/infinite.out -
	bin/mini-core examples/fixedpoint.core | diff tests/fixedpoint.out -
	bin/mini-core examples/map.core        | diff tests/map.out -
	bin/mini-core examples/fold.core       | diff tests/fold.out -
	bin/mini-core examples/freevar.core    | diff tests/freevar.out -

mini-core: src/*.hs
	cabal configure
	cabal build
	mkdir -p bin
	cp dist/build/mini-core/mini-core bin/mini-core

update: mini-core
	bin/mini-core examples/arithmetic.core > tests/arithmetic.out
	bin/mini-core examples/factorial.core  > tests/factorial.out
	bin/mini-core examples/fibonacci.core  > tests/fibonacci.out
	bin/mini-core examples/infinite.core   > tests/infinite.out
	bin/mini-core examples/fixedpoint.core > tests/fixedpoint.out
	bin/mini-core examples/map.core        > tests/map.out
	bin/mini-core examples/fold.core       > tests/fold.out
	bin/mini-core examples/freevar.core    > tests/freevar.out

loud: mini-core
	bin/mini-core examples/arithmetic.core
	bin/mini-core examples/factorial.core
	bin/mini-core examples/fibonacci.core
	bin/mini-core examples/infinite.core
	bin/mini-core examples/fixedpoint.core
	bin/mini-core examples/map.core
	bin/mini-core examples/fold.core
	bin/mini-core examples/freevar.core

pretty: mini-core
	bin/mini-core --prettyprint examples/arithmetic.core
	bin/mini-core --prettyprint examples/factorial.core
	bin/mini-core --prettyprint examples/fibonacci.core
	bin/mini-core --prettyprint examples/infinite.core
	bin/mini-core --prettyprint examples/fixedpoint.core
	bin/mini-core --prettyprint examples/map.core
	bin/mini-core --prettyprint examples/fold.core
	bin/mini-core --prettyprint examples/freevar.core

transform: mini-core
	bin/mini-core --transform examples/arithmetic.core
	bin/mini-core --transform examples/factorial.core
	bin/mini-core --transform examples/fibonacci.core
	bin/mini-core --transform examples/infinite.core
	bin/mini-core --transform examples/fixedpoint.core
	bin/mini-core --transform examples/map.core
	bin/mini-core --transform examples/fold.core
	bin/mini-core --transform examples/freevar.core
clean:
	cabal clean
	rm -f bin/mini-core
