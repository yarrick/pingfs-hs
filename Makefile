# You need the network library: install with `cabal install network`

pingfs: pingfs.hs
	ghc --make pingfs.hs -O2

run: pingfs
	sudo ./pingfs

clean:
	rm -f pingfs *.o *.hi
