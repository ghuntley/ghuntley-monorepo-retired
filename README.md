# ghuntley's personal monorepo

This repository is the [monorepo][] for my personal software.

Everything in here is built using [Nix][] with an automatic attribute-set layout
that mirrors the filesystem layout of the repository (this might feel familiar
to users of Bazel).

If you've ended up here and have no idea who I am, feel free to follow me [on
Twitter][].

# Highlights

## Tools

* `third_party/copyright-headers` idempotently add copyright headers to source files
   unless a stub `.autocopyrightignore` is found in the current or parent directory.
* `third_party/licensed` verify the licenses of dependencies.
* `third_party/nixpkgs` contains my fork of the Nix package manager packages.

## Infrastructure

* `infra/desktops` contains my nixos configs for my personal computers.

## Packages / Libraries

## Services

## Miscellaneous

Presentations I've given in the past are in the `presentations` folder, these
cover a variety of topics and some of them have links to recordings.

Workshops I've taught in the past are in the `workshops` folder, these
cover a variety of topics.

# Contributing

If you'd like to contribute to any of the tools in here, please check out the
[contribution guidelines](/tree/docs/CONTRIBUTING.md).

[monorepo]: https://en.wikipedia.org/wiki/Monorepo
[Nix]: https://nixos.org/nix
[on Twitter]: https://twitter.com/geoffreyhuntley
