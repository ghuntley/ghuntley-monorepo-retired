{ compiler ? "ghc821" }:

let
  config = {
    packageOverrides = pkgs: rec {
      haskell = pkgs.haskell // {
        packages = pkgs.haskell.packages // {
          "${compiler}" = pkgs.haskell.packages."${compiler}".override {
            overrides = haskellPackagesNew: haskellPackagesOld: rec {
               containers-unicode-symbols =
                  haskellPackagesNew.callPackage ./containers-unicode-symbols.nix { };
               project0 =
                   haskellPackagesNew.callPackage ./default.nix { };
            };
          };
        };
      };
    };
  };

  pkgs = import <nixpkgs> { inherit config; };

in
  { project0 = pkgs.haskell.packages.${compiler}.project0;
  }
