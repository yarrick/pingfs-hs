
pingfs: pingfs.hs
	ghc --make pingfs.hs -O2

run: pingfs
	sudo ./pingfs

clean:
	rm -f pingfs *.o *.hi
