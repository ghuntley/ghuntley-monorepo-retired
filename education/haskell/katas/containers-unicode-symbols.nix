{ mkDerivation, base, base-unicode-symbols, containers, fetchgit
, stdenv
}:
mkDerivation {
  pname = "containers-unicode-symbols";
  version = "0.3.1.1";
  src = fetchgit {
    url = "https://github.com/EvanMisshula/containers-unicode-symbols.git";
    sha256 = "1yq4capg5m7qks5pfr7v28zz2pxgp5imwk813wranz5zx49dfria";
    rev = "db339c32a3cbce6b092330d88fa3e25f25e8e245";
    fetchSubmodules = true;
  };
  libraryHaskellDepends = [ base base-unicode-symbols containers ];
  homepage = "http://haskell.org/haskellwiki/Unicode-symbols";
  description = "Unicode alternatives for common functions and operators";
  license = stdenv.lib.licenses.bsd3;
}
