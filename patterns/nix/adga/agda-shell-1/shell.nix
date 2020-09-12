# editorconfig-checker-disable-file
# Copyright (c) 2020 Geoffrey Huntley <ghuntley@ghuntley.com>. All rights reserved.
# SPDX-License-Identifier: Proprietary
let
  nixpkgs = builtins.fetchGit {
    url = "https://github.com/NixOS/nixpkgs-channels";
    # rev = "3b3082aa4bc18f165dd221a6a0bf7cc3f68f6163";
    rev = "6d68b920eb2df73d68a9355f9d572dbf97add5f5";
    ref = "nixos-20.03";
  };

  pkgs = import nixpkgs { };

  emacsWithPackages = (pkgs.emacsPackagesGen pkgs.emacs).emacsWithPackages
    (epkgs: (with epkgs.melpaStablePackages; [
      counsel
      ivy
      swiper
      which-key
      use-package
    ]));

  init-el = pkgs.writeText "init.el" ''
    (setq user-emacs-directory "/tmp")

    (setq inhibit-startup-message t) ;; hide the startup message
    (load-theme 'tango-dark t)       ;; load theme
    (menu-bar-mode 0)                ;; no menu bar
    (tool-bar-mode 0)                ;; no tool bar
    (scroll-bar-mode 0)              ;; no scroll bars

    (setq byte-compile-error-on-warn t)
    (setq byte-compile--use-old-handlers nil)
    (setq-default indent-tabs-mode nil)
    (add-to-list 'load-path "./")

    (require 'package)
    (package-initialize)

    ;; https://github.com/nilcons/agda-coq-setup/blob/master/docker/dot-emacs.el
    (use-package counsel
      :after ivy
      :config (counsel-mode))

    (use-package ivy
      :defer 0.1
      :diminish
      :bind (("C-c C-r" . ivy-resume)
            ("C-x B" . ivy-switch-buffer-other-window))
      :custom
      (ivy-count-format "(%d/%d) ")
      (ivy-use-virtual-buffers t)
      :config (ivy-mode))

    (use-package swiper
      :after ivy
      :bind (("C-s" . swiper)
            ("C-r" . swiper)))

    (use-package which-key
      :config
      (which-key-mode 1))

    ;; Agda mode
    (load-file (let ((coding-system-for-read 'utf-8))
             (shell-command-to-string "agda-mode locate")))

    (setq default-frame-alist '((font . "Fira Code 14")))

    (setq auto-mode-alist
    (append
     '(("\\.agda\\'" . agda2-mode)
       ("\\.lagda.md\\'" . agda2-mode))
     auto-mode-alist))
  '';

  # Fixes AgdaStdlib
  # https://kaguya.org/fix/nix-agda-stdlib.html
  standard-library = pkgs.writeText "standard-library.agda-lib" ''
    name: standard-library
    include: ${pkgs.AgdaStdlib.out}/share/agda/
  '';

  agda-libraries = pkgs.writeText "libraries" ''
    ${standard-library}
  '';

  # Pulled in from
  # https://github.com/NixOS/nixpkgs/pull/76653
  agda = pkgs.stdenv.mkDerivation {
    name = "agda-with-stuff";
    builder = pkgs.writeScript "agda" ''
      source $stdenv/setup
      mkdir -p $out/bin
      makeWrapper ${pkgs.haskellPackages.Agda}/bin/agda \
        $out/bin/agda \
        --add-flags "--library-file=${agda-libraries}"

      makeWrapper ${pkgs.haskellPackages.Agda}/bin/agda-mode \
        $out/bin/agda-mode
    '';
    buildInputs = [ pkgs.makeWrapper ];
  };

  emacs = pkgs.stdenv.mkDerivation {
    name = "emacs-with-stuff";
    builder = pkgs.writeScript "emacs" ''
      source $stdenv/setup
      mkdir -p $out/bin
      makeWrapper ${emacsWithPackages}/bin/emacs \
        $out/bin/emacs \
        --prefix PATH : ${pkgs.lib.makeBinPath [ agda ]} \
        --add-flags "--no-init-file" \
        --add-flags "--load ${init-el}"
    '';
    buildInputs = [ pkgs.makeWrapper ];
  };
in
pkgs.stdenv.mkDerivation {
  name = "agda-shell";

  buildInputs = [
    emacs
    agda
  ];

  shellHook = ''
    echo "Ready!"
  '';

}
