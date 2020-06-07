[![Build Status](https://img.shields.io/endpoint.svg?url=https%3A%2F%2Factions-badge.atrox.dev%2Fghuntley%2Fghuntley%2Fbadge%3Fref%3Dmaster&style=flat)](https://actions-badge.atrox.dev/ghuntley/ghuntley/goto?ref=master) [![Actions Panel](https://img.shields.io/badge/actionspanel-enabled-brightgreen)](https://www.actionspanel.app/app/ghuntley/ghuntley)


This repository is the [monorepo][] for my personal tools and infrastructure.
Everything in here is built using [Nix][] with an automatic attribute-set layout
that mirrors the filesystem layout of the repository (this might feel familiar
to users of Bazel).

If you've ended up here and have no idea who I am, feel free to follow me [on
Twitter][].

# Highlights

## Tools

* `third_party/copyright-headers` idempotently add copyright headers to source files
   unless a stub `.autocopyrightignore` is found in the current or parent directly.
* `third_party/licensed` verify the licenses of dependencies.
* `third_party/nixpkgs` contains my fork of the Nix package manager packages

## Packages / Libraries

## Services

Services in this repository are deployed on a Google Kubernetes Engine cluster
using [Nixery]().

* `web/blog` and `web/homepage` contain my blog and website setup
  (serving at [tazj.in][])
* `web/cgit-taz` contains a slightly patched version of `cgit` that serves my
  git web interface at [git.tazj.in][]
* `ops/journaldriver` contains a small Rust daemon that can forward logs from
  journald to Stackdriver Logging

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
