# Introduction to Nix

## 📖 Overview

This workshop provides a view of Nix, the purely functional package manager, and
the Nix expression language, which is used to define packages for Nix.

Nix can be used as a package manager on non-NixOS GNU/Linux distributions as
well as on MacOS, so this section of the tutorial is useful even if you don't
plan to run NixOS full time. If you're using a GNU/Linux distribution, Nix can be
used to install new software (or new versions** alongside system-installed
packages. If you're on MacOS, Nix can be a replacement/supplement for MacPorts
or Homebrew.

By the end of this module you will:

* Learn what the Nix package manager is, and how it improves the status quo
  over other forms of Unix packages.
* Be able to write simple Nix expressions to describe, build and install
  packages in several language platforms.

## ✋ Before You Begin

You need to have the Nix package manager installed on your computer before
proceeding. Per-platform installation instructions can be found in the [previous
module][previous-module].

## 💡 What is the Nix package manager

Every package has a hash based on everything that was used to build it, and the
package outputs are placed in an immutable store that is keyed on that hash.

The [About Nix][about-nix] chapter of the Nix manual provides a more detailed overview of the
benefits of Nix. In summary, we get these benefits:

* we have atomic upgrades and rollbacks
* we can have multiple versions of packages installed
* multiple non-privileged users installing packages without issue
* we have a simple functional language that can be used to describe these
  packages
* our package descriptions always have a complete description of their
  dependencies
* we can share a cache of build artifacts and know that we’re getting the same
  outputs as if we’d built everything from the source code ourselves

That idea can be extended to managing a whole Linux distribution (NixOS) a fleet
of machines (NixOps), but here we're looking at the package manager.

## 💡 What is the Nix expression language

The Nix expression language is a pure, lazy, functional language.

* Purity means that operations in the language don't have side-effects (for instance, there is no variable assignment). The only side effects are deliberate and controlled, and aimed at its main function of building software packages.
* Laziness means that arguments to functions are evaluated only when they are needed. 
* Functional means that functions are “normal” values that can be passed around and manipulated in interesting ways.

The language is not a full-featured, general purpose language. Its main job is to describe packages, compositions of packages, and the variability within packages.

## 📚 Additional reading material

* [Introducing Nix][introducing-nix]
* [Getting started with Nix][getting-started-with-nix]
* [Nix Manual Glossary][mix-manual-glossary]

## ⏭️ What's next

In [the next module][next-module] you'll learn about using Nix as a package manager. First
we'll recap some of the tasks you've already performed, then we'll dive into
more tasks.

<!-- in-line links -->
[about-nix]: http://nixos.org/nix/manual/#ch-about-nix
[introducing-nix]: http://qfpl.io/posts/nix/introducing-nix/
[getting-started-with-nix]: http://qfpl.io/posts/nix/getting-started-with-nix/
[mix-manual-glossary]: https://nixos.org/nix/manual/#part-glossary

[previous-module]: ../00-prerequisites/README.md
[next-module]: ../01-using-nix/README.md
