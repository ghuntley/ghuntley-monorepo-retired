# This file is used to move things from nested attribute sets to the
# top-level.
{ depot, ... }:

{
  buildGo = depot.nix.buildGo; # TODO(tazjin): remove this

  # These packages must be exposed for compatibility with buildGo.
  #
  # Despite buildGo being tracked in this tree, I want it to be possible
  # for external users to import it with the default nixpkgs layout.
  inherit (depot.third_party) go ripgrep;
}
