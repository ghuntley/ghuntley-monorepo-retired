# Mesh VPN

* Install TINC

## windows 
At an elevated prompt
```bash
cd c:\progra~2\tinc
# mkdir $vpnIdentifier
# tinc -n $vpnIdentifier init $computerName

Generating 2048 bits keys:
.............................................................................+++ p
......................................................+++ q
Done.
Generating Ed25519 keypair:
Done.

# tinc -n vpn generate-keys 4096
Generating 4096 bits keys:
.............................................................................+++ p
.............................................................................+++ q
Done.
Warning: old key(s) found and disabled.
Generating Ed25519 keypair:
Done.
Warning: old key(s) found and disabled.

# cd C:\PROGRA~2\tinc\tap-win64
# tapinstall.exe install OemWin2k.inf tap0901

```

## Linux

```bash
# mkdir /etc/tinc/$vpnIdentifier
# touch /etc/tinc/$vpnIdentifier/tinc.conf
# tincd -n vpn -K4096
Generating 4096 bits keys:
..................++ p
...................................++ q
Done.

# echo 'EXTRA="-d -n $vpnIdentifier"' > /etc/default/tinc
```
