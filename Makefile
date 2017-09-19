.PHONY: build
build:
	stack build

.PHONY: clean
clean:
	stack clean

.PHONY: example
example: build
	stack exec haskeline-brick-example

