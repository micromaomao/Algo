GHC=ghc
GHCFLAGS=

everything: mini-max-sum.bin reduced-string.bin hackerland-radio-transmitters.bin mark-and-toys.bin greedy-florist.bin tutorial-intro.bin edit-distance.bin coin-change.bin rotate-string.bin
rebuild: clean everything
clean:
	rm -f *.hi *.o *.bin
mini-max-sum.bin: mini-max-sum.hs
	$(GHC) $(GHCFLAGS) -o mini-max-sum.bin mini-max-sum.hs
reduced-string.bin: reduced-string.hs
	$(GHC) $(GHCFLAGS) -o reduced-string.bin reduced-string.hs
hackerland-radio-transmitters.bin: hackerland-radio-transmitters.hs
	$(GHC) $(GHCFLAGS) -o hackerland-radio-transmitters.bin hackerland-radio-transmitters.hs
mark-and-toys.bin: mark-and-toys.hs
	$(GHC) $(GHCFLAGS) -o mark-and-toys.bin mark-and-toys.hs
greedy-florist.bin: greedy-florist.hs
	$(GHC) $(GHCFLAGS) -o greedy-florist.bin greedy-florist.hs
tutorial-intro.bin: tutorial-intro.hs
	$(GHC) $(GHCFLAGS) -o tutorial-intro.bin tutorial-intro.hs
edit-distance.bin: edit-distance.hs
	$(GHC) $(GHCFLAGS) -o edit-distance.bin edit-distance.hs
coin-change.bin: coin-change.hs
	$(GHC) $(GHCFLAGS) -o coin-change.bin coin-change.hs
rotate-string.bin: rotate-string.hs
	$(GHC) $(GHCFLAGS) -o rotate-string.bin rotate-string.hs
