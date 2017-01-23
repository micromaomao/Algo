GHC=ghc
GHCFLAGS=

everything: mini-max-sum reduced-string hackerland-radio-transmitters mark-and-toys greedy-florist tutorial-intro
rebuild: clean everything
clean:
	rm -f mini-max-sum reduced-string hackerland-radio-transmitters mark-and-toys greedy-florist tutorial-intro *.hi *.o
mini-max-sum: mini-max-sum.hs
	$(GHC) $(GHCFLAGS) -o mini-max-sum mini-max-sum.hs
reduced-string: reduced-string.hs
	$(GHC) $(GHCFLAGS) -o reduced-string reduced-string.hs
hackerland-radio-transmitters: hackerland-radio-transmitters.hs
	$(GHC) $(GHCFLAGS) -o hackerland-radio-transmitters hackerland-radio-transmitters.hs
mark-and-toys: mark-and-toys.hs
	$(GHC) $(GHCFLAGS) -o mark-and-toys mark-and-toys.hs
greedy-florist: greedy-florist.hs
	$(GHC) $(GHCFLAGS) -o greedy-florist greedy-florist.hs
tutorial-intro: tutorial-intro.hs
	$(GHC) $(GHCFLAGS) -o tutorial-intro tutorial-intro.hs
