{ pkgs, config, lib, ... }: {
  services.journaldriver = {
    enable = true;
    logStream = "home";
    googleCloudProject = "ghuntley-infrastructure";
    applicationCredentials = "/etc/gcp/key.json";
  };
}
