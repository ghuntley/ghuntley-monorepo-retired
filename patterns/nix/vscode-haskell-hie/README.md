# Visual Studio Code with Haskell IDE Engine and GHC 8.6.5

Gives you a `nix-shell` with visual studio code ghc 8.6.5 and hie.
To use pre compiled binaries use `atopuzov.cachix.org`.

Still WIP.

## Decisions

### Stack

Stack exectuable is wrapped so it adds `--no-nix --no-docker --system-ghc` as it seems to be confused which ghc to use and since HIE is tied to ghc-8.6.5 that is the only version it can use.

## Problems encountered

### vscode-with-extensions

If I used the `vscode-with-extensions` the extensions would not work:

```
[2020-05-25 11:14:25.500] [renderer1] [error] EROFS: read-only file system, open '/nix/store/jaqz1kgvq8ik6hccwavaj7vgsdry59i2-vscode-extensions-1.45.0/share/vscode/extensions/justusadam.language-haskell/package.json': Error: EROFS: read-only file system, open '/nix/store/jaqz1kgvq8ik6hccwavaj7vgsdry59i2-vscode-extensions-1.45.0/share/vscode/extensions/justusadam.language-haskell/package.json'
[2020-05-25 11:14:25.501] [renderer1] [error] EROFS: read-only file system, open '/nix/store/jaqz1kgvq8ik6hccwavaj7vgsdry59i2-vscode-extensions-1.45.0/share/vscode/extensions/alanz.vscode-hie-server/package.json': Error: EROFS: read-only file system, open '/nix/store/jaqz1kgvq8ik6hccwavaj7vgsdry59i2-vscode-extensions-1.45.0/share/vscode/extensions/alanz.vscode-hie-server/package.json'
[2020-05-25 11:14:25.501] [renderer1] [error] EROFS: read-only file system, open '/nix/store/jaqz1kgvq8ik6hccwavaj7vgsdry59i2-vscode-extensions-1.45.0/share/vscode/extensions/bbenoist.Nix/package.json': Error: EROFS: read-only file system, open '/nix/store/jaqz1kgvq8ik6hccwavaj7vgsdry59i2-vscode-extensions-1.45.0/share/vscode/extensions/bbenoist.Nix/package.json'
```

### GLIBC mismatch

Using upstream all-hies and a newer GHC would lead to mismatch in GLIBC versions betwwen the two:
https://github.com/Infinisil/all-hies/issues/32

One option is to use ghc 8.6.5 from the same git nixpkgs commit all-hies uses (nixpkgsForGhc/ghc865) or to update it which I did in:
https://github.com/atopuzov/all-hies/commit/f8249043a53f0f8e5f5415c0b47a28e00fcf5558
