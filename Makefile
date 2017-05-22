all:
	jbuilder build @install

tests:
	jbuilder runtest

check: tests

clean:
	rm -rf _build

.PHONY: all tests doc clean check

include mk/Makefile.github
