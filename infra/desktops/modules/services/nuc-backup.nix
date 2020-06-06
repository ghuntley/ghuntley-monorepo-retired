{ pkgs, ... }: {
  systemd.services = {
    onedrive = {
      serviceConfig = {
        ExecStart =
          "${pkgs.rclone}/bin/rclone sync --transfers=16 onedrive: /mnt/data/onedrive";
        Type = "oneshot";
        User = "ghuntley";
      };
    };
  };

  systemd.services.restic-backups-onedrive-b2.unitConfig = {
    After = "onedrive.service";
    Requires = "onedrive.service";
  };

  services.restic.backups = {
    onedrive-b2 = {
      passwordFile = "/home/ghuntley/secrets/restic-b2-data";
      s3CredentialsFile = "/home/ghuntley/secrets/restic-b2-data-credentials";
      user = "ghuntley";
      paths = [ "/mnt/data/onedrive" ];
      repository = "b2:restic-smb:repos";
      extraOptions = [ "b2.connections=25" ];
      extraBackupArgs = [ "--verbose" ];
      timerConfig = { OnCalendar = "daily"; };
    };

    maildir-b2 = {
      passwordFile = "/home/ghuntley/secrets/restic-b2-data";
      s3CredentialsFile = "/home/ghuntley/secrets/restic-b2-data-credentials";
      user = "ghuntley";
      paths = [ "/home/ghuntley/mbsync" ];
      repository = "b2:restic-smb:maildir";
      extraOptions = [ "b2.connections=25" ];
      extraBackupArgs = [ "--verbose" ];
      timerConfig = { OnCalendar = "daily"; };
    };

    repos-b2 = {
      passwordFile = "/home/ghuntley/secrets/restic-b2-data";
      s3CredentialsFile = "/home/ghuntley/secrets/restic-b2-data-credentials";
      user = "ghuntley";
      paths = [ "/mnt/data/repos" ];
      repository = "b2:restic-smb:repos";
      extraOptions = [ "b2.connections=25" ];
      extraBackupArgs = [ "--exclude=/mnt/data/repos/Media" "--verbose" ];
      timerConfig = { OnCalendar = "*-*-* 03:00:00"; };
    };

    # repos-disk = {
    #   passwordFile = "/home/ghuntley/secrets/restic-b2-data";
    #   user = "ghuntley";
    #   paths = ["/mnt/data/repos"];
    #   repository = "/mnt/backup/@restic-repos";
    #   extraBackupArgs = [ "--exclude=/mnt/data/repos/Media"
    #                       "--verbose" ];
    #   timerConfig = {
    #     OnCalendar = "*-*-* 05:00:00"; #
    #   };
    # };
  };

  # systemd.services.restic-backups-repos-disk.unitConfig = {
  #   # ConditionPathIsMountPoint="/mnt/backup";
  #   After="mnt-backup.mount";
  #   Requires="mnt-backup.mount";
  # };
}
