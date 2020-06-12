{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, base-unicode-symbols, bytestring
      , containers, containers-unicode-symbols, hspec, mtl, QuickCheck
      , stdenv
      }:
      mkDerivation {
        pname = "katas";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = false;
        isExecutable = true;
        executableHaskellDepends = [ base ];
        testHaskellDepends = [
          base base-unicode-symbols bytestring containers
          containers-unicode-symbols hspec mtl QuickCheck
        ];
        homepage = "https://github.com/adomokos/hashmir#readme";
        license = "unknown";
        hydraPlatforms = stdenv.lib.platforms.none;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
