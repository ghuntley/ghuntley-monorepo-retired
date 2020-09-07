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
* `.github/auto-merge.yml` automatically merges approved pull-requests that pass all status checks.
* `.github/cla.yml` creates a comment on Pull Request asking contributors who have not signed the [CLA][] to sign and also
   fails the pull request status check with a `failure`. The contributors are requested to sign the CLA within the
   pull request by copy and pasting **"I have read the CLA Document and I hereby sign the CLA"** as a Pull Request comment.
   If the contributor has already signed the CLA, then the PR status will pass with `success`. Signatures are stored in `cla/signatures.json`.
* `.github/chatops-rebase.yml` when `/rebase` is left as a comment by a maintainer the pull-request is automatically rebased. 

## Tools

* `third_party/copyright-headers` idempotently add copyright headers to source files with the contents of `.copyrightheader`
   unless a stub `.autocopyrightignore` is found in the current or parent directory.
* `third_party/licensed` verify the licenses of dependencies.
* `third_party/nixpkgs` contains my fork of the Nix package manager packages which is automatically updated via the `dependabot` automation.

## Infrastructure

* `infra/desktops` contains my nixos configs for my personal computers.
* `infra/homelab` contains my configs for my homelab.

## Dotfiles

* `dotfiles/linux` contains my dotfiles for linux computers.
* `dotfiles/windows` contains my dotfiles for windows computers.
* `dotfiles/macos` contains my dotfiles for mac computers.

## Applications

* `dotfiles/pasteboard` your favourite macOS command line clipboard manipulation libraries `pbcopy` / `pbpaste` ported over to Windows and available
   for installation via Chocolatey.

## Browser Extensions

* `browser-extensions/serverless-to-cgi-bin` tongue in cheek browser extension for FireFox and Google Chrome that replaces occurrences of 'serverless' with 'cgi-bin'.

## BIOS / Firmware

* `bios/seaslic` If your computer is installed with Windows 7/8 by default but you'd prefer to run Linux as your desktop but on ocassion run a single
   Windows virtual machine under KVM using the activiation credentials within your computers BIOS then SeaSLIC can help you achieve this. 

   As of 6th of Apr 2014 thanks to the excellent work by Michael Tokarev this patch is now integrated by default into Debian which removes the need to
   roll and maintain your own your own copy of SeaBIOS.

## Packages / Libraries

* `libraries/python/template` is a `cookiecutter` template for creating new python libraries.
* `libraries/python/telstra-data-usage` retrieves mobile and internet data usage for Telstra services.

## Services

* `services/cachix-push-daemon.[service|nix]` automatically publish build artifacts to cachix.org so future builds of the monorepository go 🚀🚀🚀🚀🚀 

  Available as an expression NixOS but is automatically installed, configured and started as a user systemd service when the developer
  environment initialises via `de-init`

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
