build:
	ghc -prof -auto-all -caf-all -o bin/go src/*.hs

run: build
	./bin/go +RTS -xc
