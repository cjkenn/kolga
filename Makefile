# This is a mini makefile, mostly for running some debug commands. I like to have a scratch
# file used to run some programs and test things. This makefile assumes that a scratch.kol
# file exists to run some small test programs.

.PHONY: debug ast ir link clean

scratch:
	cargo run scratch.kol

debug:
	RUST_BACKTRACE=1 cargo run scratch.kol --dump-ast --dump-ir

ast:
	RUST_BACKTRACE=1 cargo run scratch.kol --dump-ast

ir:
	RUST_BACKTRACE=1 cargo run scratch.kol --dump-ir

link: scratch
	clang scratch.o -o scratch

run: scratch link
	./scratch

clean:
	rm scratch
	rm *.o
