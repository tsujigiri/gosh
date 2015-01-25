all: build run

build:
	ghc -o bin/go src/*.hs

run:
	./bin/go
