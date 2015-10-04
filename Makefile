MINI-CORE = stack exec mini-core

test: mini-core
	$(MINI-CORE) -- examples/arithmetic.core  | diff tests/arithmetic.out -
	$(MINI-CORE) -- examples/factorial.core   | diff tests/factorial.out -
	$(MINI-CORE) -- examples/fibonacci.core   | diff tests/fibonacci.out -
	$(MINI-CORE) -- examples/infinite.core    | diff tests/infinite.out -
	$(MINI-CORE) -- examples/fixedpoint.core  | diff tests/fixedpoint.out -
	$(MINI-CORE) -- examples/map.core         | diff tests/map.out -
	$(MINI-CORE) -- examples/fold.core        | diff tests/fold.out -
	$(MINI-CORE) -- examples/freevar.core     | diff tests/freevar.out -
	$(MINI-CORE) -- examples/polymorphic.core | diff tests/polymorphic.out -
	$(MINI-CORE) -- examples/badcase.core     | diff tests/badcase.out -
	$(MINI-CORE) -- examples/badscrut.core    | diff tests/badscrut.out -
	$(MINI-CORE) -- examples/badpoly.core     | diff tests/badpoly.out -

mini-core: src/*.hs
	stack build

update: mini-core
	$(MINI-CORE) -- examples/arithmetic.core  > tests/arithmetic.out
	$(MINI-CORE) -- examples/factorial.core   > tests/factorial.out
	$(MINI-CORE) -- examples/fibonacci.core   > tests/fibonacci.out
	$(MINI-CORE) -- examples/infinite.core    > tests/infinite.out
	$(MINI-CORE) -- examples/fixedpoint.core  > tests/fixedpoint.out
	$(MINI-CORE) -- examples/map.core         > tests/map.out
	$(MINI-CORE) -- examples/fold.core        > tests/fold.out
	$(MINI-CORE) -- examples/freevar.core     > tests/freevar.out
	$(MINI-CORE) -- examples/polymorphic.core > tests/polymorphic.out
	$(MINI-CORE) -- examples/badcase.core     > tests/badcase.out
	$(MINI-CORE) -- examples/badscrut.core    > tests/badscrut.out
	$(MINI-CORE) -- examples/badpoly.core     > tests/badpoly.out

loud: mini-core
	$(MINI-CORE) -- examples/arithmetic.core
	$(MINI-CORE) -- examples/factorial.core
	$(MINI-CORE) -- examples/fibonacci.core
	$(MINI-CORE) -- examples/infinite.core
	$(MINI-CORE) -- examples/fixedpoint.core
	$(MINI-CORE) -- examples/map.core
	$(MINI-CORE) -- examples/fold.core
	$(MINI-CORE) -- examples/freevar.core
	$(MINI-CORE) -- examples/polymorphic.core
	$(MINI-CORE) -- examples/badcase.core
	$(MINI-CORE) -- examples/badscrut.core
	$(MINI-CORE) -- examples/badpoly.core

pretty: mini-core
	$(MINI-CORE) -- --show-parse examples/arithmetic.core
	$(MINI-CORE) -- --show-parse examples/factorial.core
	$(MINI-CORE) -- --show-parse examples/fibonacci.core
	$(MINI-CORE) -- --show-parse examples/infinite.core
	$(MINI-CORE) -- --show-parse examples/fixedpoint.core
	$(MINI-CORE) -- --show-parse examples/map.core
	$(MINI-CORE) -- --show-parse examples/fold.core
	$(MINI-CORE) -- --show-parse examples/freevar.core
	$(MINI-CORE) -- --show-parse examples/polymorphic.core
	$(MINI-CORE) -- --show-parse examples/badcase.core
	$(MINI-CORE) -- --show-parse examples/badscrut.core
	$(MINI-CORE) -- --show-parse examples/badpoly.core

typecheck: mini-core
	$(MINI-CORE) -- --show-types examples/arithmetic.core
	$(MINI-CORE) -- --show-types examples/factorial.core
	$(MINI-CORE) -- --show-types examples/fibonacci.core
	$(MINI-CORE) -- --show-types examples/infinite.core
	$(MINI-CORE) -- --show-types examples/fixedpoint.core
	$(MINI-CORE) -- --show-types examples/map.core
	$(MINI-CORE) -- --show-types examples/fold.core
	$(MINI-CORE) -- --show-types examples/freevar.core
	$(MINI-CORE) -- --show-types examples/polymorphic.core
	$(MINI-CORE) -- --show-types examples/badcase.core
	$(MINI-CORE) -- --show-types examples/badscrut.core
	$(MINI-CORE) -- --show-types examples/badpoly.core

transform: mini-core
	$(MINI-CORE) -- --show-simple examples/arithmetic.core
	$(MINI-CORE) -- --show-simple examples/factorial.core
	$(MINI-CORE) -- --show-simple examples/fibonacci.core
	$(MINI-CORE) -- --show-simple examples/infinite.core
	$(MINI-CORE) -- --show-simple examples/fixedpoint.core
	$(MINI-CORE) -- --show-simple examples/map.core
	$(MINI-CORE) -- --show-simple examples/fold.core
	$(MINI-CORE) -- --show-simple examples/freevar.core
	$(MINI-CORE) -- --show-simple examples/polymorphic.core

clean:
	stack clean

