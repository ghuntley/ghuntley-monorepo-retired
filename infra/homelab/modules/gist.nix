{ pkgs, config, lib, ... }: {

  environment.systemPackages = with pkgs; [ gist ];

  environment.interactiveShellInit = ''
    alias gist='gist --private'
  '';
}
