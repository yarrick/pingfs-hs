# You need the network library: install with `cabal install network`
.PHONY = all run clean

all: pingfs

pingfs: pingfs.hs
	ghc --make pingfs.hs -O2 -o $@

run: pingfs
	sudo ./pingfs

clean:
	rm -f pingfs *.o *.hi
