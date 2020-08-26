# This makefile makes use of a scratch.kol file for running test kolga programs.

create-scratch:
	test -s scratch.kol || touch scratch.kol
.PHONY: create-scratch

scratch: create-scratch
	cargo run scratch.kol
.PHONY: scratch

debug: create-scratch
	RUST_BACKTRACE=1 cargo run scratch.kol --dump-ast --dump-ir
.PHONY: debug

ast: create-scratch
	RUST_BACKTRACE=1 cargo run scratch.kol --dump-ast
PHONY: ast

ir: create-scratch
	RUST_BACKTRACE=1 cargo run scratch.kol --dump-ir
.PHONY: ir

link: scratch
	clang scratch.o -o scratch
.PHONY: link

run: scratch link
	./scratch
.PHONY: run

clean:
	rm scratch
	rm *.o
.PHONY: clean
