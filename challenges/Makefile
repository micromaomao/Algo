GHC=ghc
GHCFLAGS=

everything: mini-max-sum reduced-string
clean:
	rm -f mini-max-sum reduced-string *.hi *.o
mini-max-sum: mini-max-sum.hs
	$(GHC) $(GHCFLAGS) -o mini-max-sum mini-max-sum.hs
reduced-string: reduced-string.hs
	$(GHC) $(GHCFLAGS) -o reduced-string reduced-string.hs
