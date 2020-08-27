# nix-shell for agda and agda standard library

Uses a pinned version of nixpkgs which has Agda 2.6.0.1 and standard-library 1.1.
Emacs is available and set up to use agda.

This should work with PLFA.

To use the standard-library by default either put it in `~/.agda/defaults` or create a `.agda-lib` file with the contents similar to:
```
name: my-agda-lib
depend: standard-library
include: .
```
