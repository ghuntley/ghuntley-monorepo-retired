# Overview

# Adding a new command line tool

1. Use https://search.nixos.org/ to identify the package name and expose in `${REPO_ROOT}/third_party/default.nix`. 
1. Create symbolic link from `${REPO_ROOT}/third_party/__dispatch.sh` to the `toolname`
1. Update the switch expression in `${REPO_ROOT}/third_party/__dispatch.sh`

# Monkey patching a command line tool

1. `${REPO_ROOT}/overrides/*`

# Additional Reading

