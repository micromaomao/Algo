GHC=ghc
GHCFLAGS=

everything: mini-max-sum reduced-string hackerland-radio-transmitters
rebuild: clean everything
clean:
	rm -f mini-max-sum reduced-string hackerland-radio-transmitters *.hi *.o
mini-max-sum: mini-max-sum.hs
	$(GHC) $(GHCFLAGS) -o mini-max-sum mini-max-sum.hs
reduced-string: reduced-string.hs
	$(GHC) $(GHCFLAGS) -o reduced-string reduced-string.hs
hackerland-radio-transmitters: hackerland-radio-transmitters.hs
	$(GHC) $(GHCFLAGS) -o hackerland-radio-transmitters hackerland-radio-transmitters.hs
