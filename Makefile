test:
	runhaskell Test.hs | diff Test.out -

loud:
	runhaskell Test.hs

