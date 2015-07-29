# You need the network library: install with `cabal install network`
# You need the HFuse library: install with `cabal install hfuse`
.PHONY = all run clean

all: pingfs

pingfs: pingfs.hs Icmp.hs
	ghc --make -threaded pingfs.hs -O2 -o $@

run: pingfs
	sudo ./pingfs foo hosts

clean:
	rm -f pingfs *.o *.hi
