{
  users.users.nixBuild = {
    name = "nixBuild";
    openssh.authorizedKeys.keys = [
      "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQDab+n4WtwJIHf2kgGLD7WkXV6pIoC73vFMsIfs1y5vEiDgP/kkjbQZuZN19ptaJRCVCFw7wwX1RHzqm6WLAYNySYvDTGvTqkexEfDH/Lx+d3TiqmAV3kPqtsIdH9Shvr7/4Q+VprkbYabUYTg7s/c5FSDzhfDOXids6Ct2STJfmxFmZRsbo0xAydIhvV8r9aAgLC/uvYIiOMsadWH6cJr3IoyxCaEVNG8mGNITobLVoN0F03r4PqwoyYdFJ4KPYR2WGyAXcw9XeRFIGfjkmlbMH8Xc1tg3HMwTMZcKUlNlnD9tmPWvofZ3n6OwwYcE40+fivHrsVMGmg1X4ZjqOK3OhdTYb0RSqrkd4vm5z+NwLJO4K3sHtMJOwTWmi2yXVeSDf3qd1FdbEgG/0rLRaQI4UjID2uYrXEjmtqk2yLsei1n5aXPcJprrYkT9reab6+4IxIwpjiz+FZw8NoeqkpfjIgzWXu/OyORq3Te22AujcmNSGQ4MFoTUlrfJ5RlTZw1ezECo6aSDnAy99Hq/dIdgi50Ceamx4zLf3IORtbtqqHeID4scDXuEKgS38+bCCuVapTlyMgdBMkEYW0ksIuoEWOoOrvanslYMPVShkhNix6gcSjNI+KgKikvBU34jJwGCABNt5EM9jq5sQ5E/jI7ewlsiQaivb2hxA8Kw6V9I6w== ghuntley@pocket"
    ];
  };
  nix.trustedUsers = [ "nixBuild" ];

  nix.sshServe.enable = true;
  nix.sshServe.keys = [
    "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQDFqXLmL2FVGAkSlndgqaEDx0teA6Ai1wLu21KSdcBnV6XldetAHZ8AAeodgEqIYD/sO69xCm9Kwa3DbktdMO28MO6A7poQ4jvDVHray7mpsm3z5xgc1HAadjNUBvlPjPBbCvZkhcI2/MSvVknl5uFXeH58AqaIq6Ump4gIC27Mj9vLMuw7S5MoR6vJgxKK/h52yuKXs8bisBvrHYngBgxA0wpg/v3G04iplPtTtyIY3uqkgPv3VfMSEyOuZ+TLujFg36FxU5I7Ok0Bjf8f+/OdE41MYYUH1VPIHFtxNs8MPCcz2Sv0baxEhAiEBpnWsQx8mBhxmQ/cK4Ih2EOLqPKR ghuntley"
  ];
}
