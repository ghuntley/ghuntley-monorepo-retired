# Copyright (c) 2020 Geoffrey Huntley <ghuntley@ghuntley.com>. All rights reserved.
# SPDX-License-Identifier: Proprietary

{ pkgs, config, lib, ... }: {

  networking.firewall.allowedTCPPorts = [ 7080 ];

  virtualisation.oci-containers.containers = {

    sourcegraph = {
      image = "sourcegraph/server:3.20.0";
      ports = [
        "0.0.0.0:7080:7080"
      ];
      volumes = [
        "/srv/sourcegraph/etc:/etc/sourcegraph"
        "/srv/sourcegraph/data:/var/opt/sourcegraph"
      ];
      environment = {
        SRC_SYNTECT_SERVER = "http://127.0.0.1:9238";
      };
      cmd = [
      ];
    };

    syntect_server = {
      image = "sourcegraph/syntech_server";
      ports = [
        "127.0.0.1:9238:9238"
      ];
      volumes = [
      ];
      environment = { };
      cmd = [
      ];
    };

  };

}
