# Copyright (c) 2020 Geoffrey Huntley <ghuntley@ghuntley.com>. All rights reserved.
# SPDX-License-Identifier: Proprietary

{
  services.transmission.enable = true;
  networking.firewall.allowedTCPPorts = [ 51413 ];
}
