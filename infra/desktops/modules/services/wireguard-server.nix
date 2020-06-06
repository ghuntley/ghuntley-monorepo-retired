{
  boot.kernel.sysctl = { "net.ipv4.ip_forward" = true; };
  networking = {
    nat = {
      enable = true;
      externalInterface = "eth0";
      internalInterfaces = [ "wg0" ];
    };
    firewall = {
      allowedUDPPorts = [ 51820 ];
      extraCommands = ''
        iptables -t nat -A POSTROUTING -s 10.100.0.0/24 -o eth0 -j MASQUERADE
        iptables -A FORWARD -i wg0 -j ACCEPT
        iptables -A FORWARD -o wg0 -j ACCEPT
      '';
    };
    wireguard.interfaces = {
      wg0 = {
        ips = [ "10.100.0.1/24" ];
        listenPort = 51820;
        privateKeyFile = "/root/wireguard-keys/private";
        peers = [{
          publicKey = "w20VAhW/yZ4D01D/9Y9NwtS0Lwk6QLTm7xc3zegFwms=";
          presharedKeyFile = "/root/wireguard-keys/psk";
          allowedIPs = [ "10.100.0.2/32" ];
        }];
      };
    };
  };
}
