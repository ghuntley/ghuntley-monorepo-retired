# This file defines the derivations that should be built by CI.
#
# The plan is still to implement recursive tree traversal
# automatically and detect all derivations that have `meta.enableCI =
# true`, but this is currently more effort than it would save me.

with (import ./default.nix {}); [
  web."ghuntley.com"
 ]
 
