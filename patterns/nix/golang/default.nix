{ buildGoPackage, lib }:

buildGoPackage rec {
  pname = "nix-patterns-golang";
  version = "0.0.0";
  goDeps = ./.nix/deps.nix;
  src = lib.sourceFilesBySuffices ./. [ ".go" ".mod" ".sum" ];

  goPackagePath = "github.com/ghuntley/depot/patterns/nix/golang";
  subPackages = [ "cmd/demo" ];
  allowGoReference = false;
  doCheck = true;

  meta = {
    description = ''
      Basic demonstration of a Go application built with Go modules and Nix.
    '';
    license = lib.licenses.mpl20;
    homepage = "https://github.com/ghuntley/depot/tree/trunk/patterns/nix/golang";
  };
}
