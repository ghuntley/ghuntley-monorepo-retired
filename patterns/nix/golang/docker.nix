{ system ? builtins.currentSystem
, pkgs ? import ./.nix/nixpkgs.nix { 
    inherit system; 
    config = { allowUnsupportedSystem = true; };
  }
}:

let
  demo = pkgs.callPackage ./default.nix { };
in
  pkgs.dockerTools.buildImage {
    name = "ghuntley/nix-patterns-golang";
    tag = demo.version;
    created = "now";

    contents = [ demo ];
  
    config = {
      Cmd = [ "/bin/demo" ];
      WorkingDir = "/";
    };
  }
