---
title: Git
layout: notes
---

# duplicating a repository

```bash
$ git clone --bare https://github.com/exampleuser/old-repository.git
$ cd old-repository.git
$ git push --mirror https://github.com/exampleuser/new-repository.git
$ cd ..
$ rm -rf old-repository.git
```

# signed commits

See also https://github.com/drduh/YubiKey-Guide

## install pre-req software

```bash
$ choco install gnu4win -y
$ choco install yubikey-manager -y
$ choco install keybase -y
```

## install and configure keybase

- create your keybase account
- [optional] verify on github, twitter and your personal website if you desire


```
$ keybase pgp gen
```

- enter your email address as used on github. If you are using a [anonymous GitHub email](https://help.github.com/en/articles/about-commit-email-addresses) you should enter both your normal and anonymous email in this step.
- store the private key to keybase when asked
- copy the public key to clipboard

## configure gpg

If you had existing keys due to failed attempts:

```
$ gpg --list-keys

for failedattempts in attempts
    gpg --delete-secret-keys <attempt.ID>
    gpg --delete-key <attempt.ID>
done
```

You'll need to wait about five minutes for the keys to propigate then run this command

```
# Import the public key
$ keybase pgp export | gpg --import

# Import the private key
$ keybase pgp export -s | gpg --allow-secret-key-import --import
```

## configure github

```
$ gpg --armor --export <your-email-address> > publickey.asc
```

- goto your github profile
- goto ssh and gpg keys
- add gpg key
- paste in the public key from the file that you exported

## configure your yubi key

```
$ gpg --card-edit
```

- Configure your gender identity with the `sex` command
- Configure your real name with the `name`

The Yubikey ships with two default PINs, one for administrative use and one for daily use (i.e. unlocking your key for signing/decryption).

* Default PIN = 123456
* Default Admin PIN = 12345678

```
gpg/card> admin
gpg/card> passwd

gpg: OpenPGP card no. <redacted> detected

1 - change PIN
2 - unblock PIN
3 - change Admin PIN
4 - set the Reset Code
Q - quit
```

- First change the PIN (remember it needs to be 6-digits long).
- If you did it right it will say "PIN changed".
- Now change the Admin PIN (needs to be 8-digits lon)
- If you did it right it will say "PIN changed".


## choose your path / threat model

### yubikey must be plugged in and physical interaction required

Yubikey has a touch feature that enables a second layer of protection when using a private key stored on the device. The access to the GPG keys will only be possible if you physically trigger the touch sensor, which denies malware or remote access scenarios. You have 15 seconds to press your key or the signing will timeout. The touch sensor can be configured with the following parameters:

- off: touch is disabled
- on: touch is enabled
- fixed: touch is enabled and can not be disabled unless a new private key is generated or imported.

```
$ cd C:\progra~1\Yubico\YubiKey Keymanager
$ ykman openpgp touch aut fixed
$ ykman openpgp touch enc fixed
$ ykman openpgp touch sig fixed
```

### yubikey must be plugged in and no interaction required

```
$ cd C:\progra~1\Yubico\YubiKey Keymanager
$ ykman openpgp touch aut off
$ ykman openpgp touch enc off
$ ykman openpgp touch sig off
```

## move secret key onto the yubikey

```bash
$ gpg --edit-key <your@email-address.com>

gpg> toggle
gpg> key 1
gpg> keytocard

Please select where to store the key: 1 (Signing)

gpg> key 1
gpg> key 2

gpg> keytocard

Please select where to store the key: 2 (Encryption)

gpg> key 2
gpg> key 3

gpg> keytocard

Please select where to store the key: 3 (Authentication)

gpg> save
```

## configure your git client

Find the identifier of your key. ie in my case it is `13DAEF2E995C1055A87C0AB71E0F156E39D1D3A6`

```
$ gpg --list-keys
---------------------------------------------------
pub   rsa4096 2019-05-24 [SC] [expires: 2035-05-20]
      13DAEF2E995C1055A87C0AB71E0F156E39D1D3A6
uid           [ unknown] Geoffrey Huntley <ghuntley@ghuntley.com>
sub   rsa4096 2019-05-24 [E] [expires: 2035-05-20]
sub   rsa2048 2019-05-24 [S] [expires: 2019-06-09]
```

Wire it into your gitconfig and configure your gpg+commit stanzas as appropriate

```bash
$ vi ~/.gitconfig
[credential]
	helper = manager
[filter "lfs"]
	clean = git-lfs clean -- %f
	smudge = git-lfs smudge -- %f
	process = git-lfs filter-process
	required = true
[gpg]
	program = c:\\Program Files (x86)\\GnuPG\\bin\\gpg.exe
[commit]
	gpgsign = true
[user]
	signingkey = 13DAEF2E995C1055A87C0AB71E0F156E39D1D3A6
	email = ghuntley@ghuntley.com
	name = Geoffrey Huntley
```

and that's it. Phew.

## future scenario a - new machine and have access to yubikey

```bash
choco install gnu4win -y
```


## future scenario b - new machine and don't have access to yubikey


```bash
choco install gnu4win -y
choco install keybase -y
```

Download/install your public and private on the computer

```
keybase pgp export | gpg --import
keybase pgp export -q <your-email-address> --secret | gpg --import --allow-secret-key-import
```

After you have finished, make sure you remove your private key from the computer.

```bash
gpg --delete-secret-keys <your-email-address>
```

# Shallow clones
fast clones without the history

`git clone $repo --depth=1`


{{<
tweet 1033084211605979137 >}} 
