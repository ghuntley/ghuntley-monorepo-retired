---
title: ssh
layout: notes
---

# SSHFP records

There is a handy command to generate the SSHFP records. Type in `ssh-keygen -r <your-domain-name>` and the magic of ssh-keygen command will print the necessary DNS records that you have to put into your DNS server.

If you have multiple key types, and want to limit SSHFP records to a certain set of keys, you can do so with the -f option. For example, `ssh-keygen -r <your-domain-name> -f /etc/ssh/ssh_host_ecdsa_key.pub` will print the SSHFP records for the public key `/etc/ssh/ssh_host_ecdsa_key.pub`.

The output would look like this:

```
example.com IN SSHFP 4 1 1b69784a17572ee7e850107f6b3052699a953ad3
example.com IN SSHFP 4 2 b11000d176792edf9c7645aebb2ccdd4a6eb0cf856cadcb6b82ee18e333bef13
```

`example.com IN` is part of the BIND and BIND-like DNS record syntax. If you are using any other DNS provider, the part after `SSHFP` is what you should copy-pasta. The record type is pretty basic. It's [Algorithm] [Fingerprint type] [Hex fingerprint]. 
