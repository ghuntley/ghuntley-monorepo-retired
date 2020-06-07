{ pkgs, ... }: {

  users.extraUsers.root = {
    shell = pkgs.zsh;
    openssh.authorizedKeys.keys = [ "" ];
  };
}
