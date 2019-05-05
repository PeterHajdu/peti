p: *.idr
	idris -p contrib -p iterm Main.idr -o p

test: *.idr
	idris -p contrib -p specdris -p iterm Test.idr -o test
	./test

clean:
	rm -f p test *.ibc *.core

.phony: clean
