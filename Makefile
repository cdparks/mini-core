test: mini-core
	bin/mini-core examples/arithmetic.core | diff tests/arithmetic.out -
	bin/mini-core examples/factorial.core  | diff tests/factorial.out -
	bin/mini-core examples/fibonacci.core  | diff tests/fibonacci.out -
	bin/mini-core examples/infinite.core   | diff tests/infinite.out -
	bin/mini-core examples/fixedpoint.core | diff tests/fixedpoint.out -


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

clean:
	cabal clean
	rm -f bin/mini-core
