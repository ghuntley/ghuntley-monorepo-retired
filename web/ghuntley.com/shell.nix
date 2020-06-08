with import <nixpkgs> {};

mkShell rec {
  name = "dotnet";
  buildInputs = [
    (with dotnetCorePackages; combinePackages [ sdk_3_1 ])
  ];
}
