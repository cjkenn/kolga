.PHONY: debug link clean

debug:
	RUST_BACKTRACE=1 cargo run scratch.kol

link: debug
	clang scratch.o -o scratch

clean:
	rm scratch
	rm *.o
