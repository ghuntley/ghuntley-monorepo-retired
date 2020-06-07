# Introducing nixpkgs and channels

## 📖 Overview

Nixpkgs is the name of the git repository where Nix packages are developed.

There are two ways to install packages with Nixpkgs: with the git checkout, or
by using channels.

* the [git checkout](https://github.com/nixos/nixpkgs) is the bleeding edge,
  up-to-the-minute updated version of the packages. The checked out git
  repository is ideal for people who want to use the latest and greatest
  available version of a package, for those who want to test things out, fix
  bugs, etc.

* [Channels](https://nixos.org/channels/) on the other hand, are snapshots built
  from tags of the `nixpkgs` git repository at an earlier version, packaged as
  independent nix repositories. Official Nix channels are usually tagged with
  a year and a month (`19.03`), and correspond to NixOS releases.
  
  Because they only receive updates bug and security fixes, release channels are
  more stable and less liable to require a lot of downloads and dependency
  changes when you upgrade your system or your use profile.

  There's also the `unstable` Nix channel, which tracks the `master` branch of
  `nixpkgs`, for those who want to live on the bleeding edge. We'll soon see how
  a balance can be achieved between the safety of a release channel and the
  newness of `unstable`.

Nix is a source-based package manager and NixOS is a source-based distribution,
therefore the Nixpkgs repo (plus the upstream sources it references) is enough
for you to install any package in it. In fact, channels are also source-based,
containing only package definition files (called `derivations` in the Nix
universe) and patches, and no built binaries. If all we used were channels, your
system would download the derivations in it, download the referenced source
files, download and install any dependency, and use it all to build a new
package.

But because nobody wants to spend all their time compiling, Nix and NixOS feature
precompiled packages. We've already seen them before: we called them
`binaryCaches` when writing them into `configuration.nix` and `substituters`
when passing them as an option to `nixos-rebuild`.

The name for these sources of precompiled packages should give us a clue: they
are "cached" builds or "substitutions", and as such they are independent of
package sources, in the sense that you will be able to install packages without
The fact that binary artifacts in Nix are named for the hash of their inputs
(the derivation they are built from) makes it easy to match a local source
package to a remote cached build.


## Introducing nixpkgs

## Introducing channels

Installing packages via channels is nicer, because the commands to install
packages with it are more convenient. The trade-off is that the packages will be
out-of-date by a few days. If you’re fine with it, then use channels instead of
the git checkout.

Channels are labeled `stable`, `unstable`, or with a specific version number,
e.g., `18.09` or `19.03`. The `stable` channel is an alias for the latest
numbered version, and the `unstable` one tracks the latest commit of the Nixpkgs
repository.

All channels are mutable. Even so-called `stable` will receive security and bug
fixes. Therefore, the same as `unstable` and older release channels before it,
it will have the `nixpkgs` branch's a commit hash for a unique identifier.

(INSERT IMAGE FROM GRUB GENERATIONS)

For this workshop, let’s use the unstable channel—it’s not as dated as stable,
nor as recent as the git checkout. To view available channels, subscribe to the
unstable channel and update its definitions, run:

```bash
$ nix-channel --list                                                       #01
$ sudo nix-channel --list                                                  #02
$ nix-env -iA nixos.glxinfo                 # or nix-env --install -A      #03
$ readlink -f `which glxinfo`                                              #04
$ nix-channel --add https://nixos.org/channels/nixpkgs-unstable nixpkgs    #05
$ nix-channel --list                                                       #06
$ nix-env -iA nixpkgs.glxinfo               # ***this will error out***    #07
$ nix-channel --update                      # or nix-env -u                #08
$ nix-env -iA nixpkgs.glxinfo               # ****this will succeed****    #09
$ readlink -f `which glxinfo`                                              #10
```

This performs the following actions:
1. list the available channels. There doesn't appear to be any channel!
2. confirm there exists a default channel that we installed system-wide when we
   installed NixOS. The fact that it's called `nixos` is just a convention; it
   could be any other name, but why make life complicated.
3. install from the `nixos` channel despite not having configured it in
   our user profile
4. let's see the actual binary in our store, and take note of the hash 
5. add the channel labeled nixpkgs-unstable from nixos.org and install it to our
   user profile under the name `nixpkgs`. This name is, again, a common
   convention. You could call it something else if you wanted to.
6. list the available channels, to check what we just did worked.
7. attempt to install the `glxinfo` package again, this time from `nixpkgs`.
   This will fail because we `add` ed the channel but did not update it. So we
   have a channel defined in `~/.nix-channels`, but we haven't fetched its
   definitions yet.
8. fetch the latest package definitions from `nixpkgs`. In this case, it's also
   the first fetch.
9. install `glxinfo` package from `nixpkgs`, this time successfully. It will
   also bring a brace of dependencies, because every package in nixpkgs is a
   closure of its dependencies, so different binaries in a NixOS system can depend 
   on different versions of the same library, without any conflict.
10. let's see the actual binary in our store, and take note of the different hash.

To browse the list of channels, go [here](https://nixos.org/channels/).

Using the example above, to install emem, run the following commands for NixOS and other systems, respectively:

```bash
nix-env -iA nixos.emem
nix-env -iA nixpkgs.emem
```

To update your channels, run:

```bash
nix-channel --update
```

You can roll back a channel update if new package definitions aren't to your liking:

```bash
nix-channel --roll-back
```

# Activities

1. Clone nixpkgs, install a package from it.
1. Visit list of available channels website
1. Show configured channel, add unstable channel, `nix-channel --update`, install package from unstable.
1. To uninstall a package via $ nix-env -e emem
1. To list all your installed packages, $ nix-env -q --installed
1. To list all available packages, $ nix-env -q --available
1. This tutorial covers Nix 1.x, because the transition to Nix 2 isn't comnplete

## 📚 Additional reading material

* [A Gentle Introduction to the Nix Family](https://ebzzry.io/en/nix)

## ⏭️ What's next
