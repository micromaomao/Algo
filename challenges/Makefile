GHC=ghc
GHCFLAGS=

everything: mini-max-sum.bin reduced-string.bin hackerland-radio-transmitters.bin mark-and-toys.bin greedy-florist.bin tutorial-intro.bin
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
