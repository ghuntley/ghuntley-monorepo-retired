{ pkgs, ... }:

pkgs.terraform_0_12.withPlugins(p: [ 
    p.acme
    p.auth0
    p.aws
    p.azurerm
    p.cloudflare
    p.gitlab
    p.github
    p.google
    p.google-beta
    p.heroku
    p.kubernetes
    p.postgresql
    p.netlify
    p.nixos
])