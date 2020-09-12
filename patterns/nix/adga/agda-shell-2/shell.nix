# editorconfig-checker-disable-file
# Copyright (c) 2020 Geoffrey Huntley <ghuntley@ghuntley.com>. All rights reserved.
# SPDX-License-Identifier: Proprietary
let
  nixpkgs = builtins.fetchGit {
    url = "https://github.com/NixOS/nixpkgs";
    rev = "354798437a8dc3d9ab1d1f5787209df0ac5ef742";
    ref = "master";
  };
  pkgs = import nixpkgs { };

  # After https://github.com/NixOS/nixpkgs/pull/76653
  # makes it pretty easy to use the agda standard library
  agda = pkgs.agda.withPackages (p: [ p.standard-library ]);

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
  '';

  emacs = pkgs.stdenv.mkDerivation {
    name = "emacs-with-stuff";
    builder = pkgs.writeScript "emacs" ''
      source $stdenv/setup
      mkdir -p $out/bin
      makeWrapper ${emacsWithPackages}/bin/emacs \
        $out/bin/emacs \
        --prefix PATH : ${pkgs.lib.makeBinPath [ agda ]} \
        --add-flags "-q -l ${init-el}"
    '';
    buildInputs = [ pkgs.makeWrapper ];
  };
in
pkgs.stdenv.mkDerivation {
  name = "agda-sandbox";

  buildInputs = [ agda emacs ];

  shellHook = ''
    echo "Ready!"
  '';
}
