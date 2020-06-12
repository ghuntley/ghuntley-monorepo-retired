{ mkDerivation, base, base-unicode-symbols, bytestring, containers
, containers-unicode-symbols, hspec, mtl, QuickCheck, stdenv
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
}
