# ghuntley's personal monorepo [![Open in Gitpod](https://gitpod.io/button/open-in-gitpod.svg)](https://gitpod.io/#https://github.com/ghuntley/ghuntley)

This repository is the [monorepo][] for my personal software.

Everything in here is built using [Nix][] with an automatic attribute-set layout
that mirrors the filesystem layout of the repository (this might feel familiar
to users of Bazel).

If you've ended up here and have no idea who I am, feel free to follow me [on
Twitter][].

# Highlights

## Automations

* `.imgbotconfig` configures `imgbot` to crawl through all image files and losslessly compress them via pull-requests.
* `.github/dependabot.yml` configures `dependabot` to automatically update git-submodules in `third_party` via pull-requests.
* `.github/auto-approve.yml` automatically approves pull-requests raised by `imgbot` or `dependabot`.
* `.github/auto-merge.yml` automatically merges approved pull-requests.
* `.github/cla.yml` creates a comment on Pull Request asking contributors who have not signed the [CLA][] to sign and also
   fails the pull request status check with a `failure`. The contributors are requested to sign the CLA within the
   pull request by copy and pasting **"I have read the CLA Document and I hereby sign the CLA"** as a Pull Request comment.
   If the contributor has already signed the CLA, then the PR status will pass with `success`. Signatures are stored in `cla/signatures.json`.
* `.github/chatops-rebase.yml` when `/rebase` is left as a comment by a maintainer the pull-request is automatically rebased. 

## Tools

* `third_party/copyright-headers` idempotently add copyright headers to source files with the contents of `.copyrightheader`
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
[contribution guidelines](/tree/docs/CONTRIBUTING.md)

[CLA]: CLA/README.md
[monorepo]: https://en.wikipedia.org/wiki/Monorepo
[Nix]: https://nixos.org/nix
[on Twitter]: https://twitter.com/geoffreyhuntley
