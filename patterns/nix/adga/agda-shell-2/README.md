# nix-shell for agda and agda standard library

Uses a pinned version of nixpkgs (unstable) which has Agda 2.6.1 and standard-library 1.3 and has much nicer agda package support.
Emacs is available and set up to use agda.

This will NOT with PLFA.

To use the standard-library by default either put it in `~/.agda/defaults` or create a `.agda-lib` file with the contents similar to:
```
name: my-agda-lib
depend: standard-library
include: .
```
