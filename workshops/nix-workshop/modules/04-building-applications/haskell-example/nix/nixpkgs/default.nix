let gitinfo = builtins.fromJSON (builtins.readFile ./git.json);
in import (builtins.fetchGit {
  inherit (gitinfo) url rev;
})
