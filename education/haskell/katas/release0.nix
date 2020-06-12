let
  config = {
    packageOverrides = pkgs: rec {
      haskellPackages = pkgs.haskellPackages.override {
        overrides = haskellPackagesNew: haskellPackagesOld: rec {
          containers =
            haskellPackagesNew.callPackage ./containers.nix { };
          containers-unicode-symbols =
            haskellPackagesNew.callPackage ./containers-unicode-symbols.nix { };
          project0 =
            haskellPackagesNew.callPackage ./default.nix { };

        };
      };
    };
  };

  pkgs = import <nixpkgs> { inherit config; };

in
  {
    containers = pkgs.haskellPackages.containers;           
    containers-unicode-symbols = pkgs.haskellPackages.containers-unicode-symbols;           
    project0 = pkgs.haskellPackages.project0;
  }
