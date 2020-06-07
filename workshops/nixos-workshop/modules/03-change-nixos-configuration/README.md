# Change NixOS Configuration

## 📖 Overview

As described in the introduction, NixOS is a GNU/Linux distribution with a
declarative system configuration manager. The whole operating system is built by
the Nix package manager from a description in a purely functional language, also
called Nix. For the purpose of this section, we'll consider the Nix language as a black box,
and we'll treat `configuration.nix` as a configuration file with a slightly
alien syntax.

For those at the Melbourne :: C◦mp◦se 2019 workshop:

* We've already changed the configuration in our NixOS system to ensure we can
  use mdns and to set up a local proxy-cache to download packages from. We did
  it in two steps: we edited the `configuration.nix` file and we ran the
  `nixos-rebuild` command.

By the end of this module you will have:

* Saved your progress so far using the DevOps principal of "Infrastructure as Code".
* Further customized your operating system configuration.
* Tested your operating system configuration.
* Applied your operating system configuration.
* Committed ("switched") your operating system configuration.
* Learned about generations and discovered how to rollback to a last good via GRUB.

## 🎯 Infrastructure as Code

Let's save your progress so far! Run the following command to make git available.

```bash
nix-env -iA nixos.git
git config --global user.email "your.name@example.com"
git config --global user.name "Your Name"
```

This command did not globally install git; instead git is installed into
`~/.nix-profile/bin/` of the root user. Here you are reaping the rewards of
NixOS not following the [Filesystem Hierarchy Standard][fhs-standard]. On other
GNU/Linux distributions git would have been installed globally and that would be
the system wide version for all users.

With NixOS, you can side-by-side multiple versions of the same software for any
user without stomping on other users. You get one of the benefits of docker, but
without overhead, complexity and instability.

Let's initialize a git repository and create a checkpoint:

```bash
cd /etc/nixos
git init
git add -A
git commit -m "initial commit"
```

## 🎯 Customising NixOS

Let's further customise NixOS:

```bash
nix-env -iA nixos.vim # alternatively `nix-env -iA nixos.nano`, or `nix-env -iA nixos.emacs`

# Or, you know, use `nano` or whatever else you might prefer.
vi /etc/nixos/configuration.nix
```

Enable X11 with KDE5 and network manager:

```nix
networking.networkmanager.enable = true;
services.xserver.enable = true;
services.xserver.displayManager.sddm.enable = true;
services.xserver.desktopManager.plasma5.enable = true;
```

In addition to these core configuration items, you might want to install some
packages to get you started. Your NixOS install is currently really bare. You
can search for packages to install with `nix-env -qaP | grep $PACKAGE`. Add any
additional packages that catch your fancy.

Add the following packages at minimum to your configuration:

```nix
environment.systemPackages = with pkgs; [
  firefox
  git
  htop
  networkmanagerapplet
  nix-prefetch-scripts
  vim
  wget
  which
  tmux
];
```

Finally, it’s not a good idea to use root all the time, so let's create a
`workshop` (or whatever other name you want) user with a home directory and add
the user to a few groups. Most importantly, let's add the user to be a member of
`wheel` so that the account can run privileged commands (`nixos-rebuild` and
`reboot`) with sudo.

```nix
users.extraUsers.workshop = {
  createHome = true;
  extraGroups = ["wheel" "video" "audio" "disk" "networkmanager"];
  group = "users";
  home = "/home/workshop";
  isNormalUser = true;
  uid = 1000;
};
```

Save your configuration and set a password for the workshop account

```bash
passwd workshop
```

This will fail, because the user doesn't exist yes. The next section explains
why and how to fix this.

## 💡 nixos-rebuild

Changes to `/etc/nixos/configuration.nix` are not applied unless `nixos-rebuild`
is invoked. This command takes a NixOS system in a working state and a
`configuration.nix` file, and returns a changed working state according to the
configuration file.

Here are the most common sub-commands:

* `nixos-rebuild switch`: Build and activate the new configuration, and make it
  the boot default. That is, the configuration is added to the GRUB boot menu as
  the default menu entry, so that subsequent reboots will boot the system into
  the new configuration. Previous configurations activated with nixos-rebuild
  switch or nixos-rebuild boot remain available in the GRUB menu.
* `nixos-rebuild boot`: Build the new configuration and make it the boot default
  (as with nixos-rebuild switch), but do not activate it. That is, the system
  continues to run the previous configuration until the next reboot.
* `nixos-rebuild test`: Build and activate the new configuration, but do not add
  it to the GRUB boot menu. Thus, if you reboot the system (or if it crashes),
  you will automatically revert to the default configuration (i.e. the
  configuration resulting from the last call to nixos-rebuild switch or
  nixos-rebuild boot).
* `nixos-rebuild build`: Build the new configuration, but neither activate it
  nor add it to the GRUB boot menu. It leaves a symlink named result in the
  current directory, which points to the output of the top-level "system"
  derivation. You do not need to be root to run nixos-rebuild build.

Other sub-commands are useful for debugging by performing dry runs or building
qemu virtual machines. Check out `man nixos-rebuild` if you want to learn more.

The `nixos-rebuild` command also has options. Here are the most common:

* `--upgrade`: fetches the latest version of NixOS from the NixOS channel before
  rebuilding.
* `--install-bootloader`: use to reinstall the bootloader after changing the
  device configuration.
* `--no-build-nix`: usually, nixos-rebuild first builds the latest version of
  the Nix package manager before rebuilding. This option disables this.
* `--fast`: equivalent to `--no-build-nix --show-trace`. Use when you're calling
  `nixos-rebuild` frequently.
* `--rollback`: Don't build a new configuration based on
  `/etc/nixos/configuration.nix`. Rather, roll back the previous configuration.

There's also additional options allow you to declare remote builders, change
profiles (GRUB submenus) to install your new configuration at, and even target a
different host to activate the new configuration on. As always, check `man
nixos-rebuild`.

## 🎯 Reboot NixOS and interrupt booting when at the GRUB menu

Now you can reboot your computer. We'll wait.

<waiting expectantly...>

When GRUB appears, navigate to `NixOS - All Configurations`

![The NixOS GRUB Menu](grub-menu.png)

Press `Enter` on your keyboard and you'll see a single entry:

![NixOS Grub Menu - Configuration 1 selected](grub-menu-one-generation.png)

Each time a new operating system configuration applied, a new menu item in GRUB
will be created. These configurations are also known as "generations". As you
are yet to apply your new operating system configuration you'll only see one
generation.

Select `NixOS - Configuration 1` and press enter to boot back into NixOS.

## 🎯 Build the operating system configuration

Login as `root` and let's validate that `/etc/nixos/configuration.nix` has no
errors:

```bash
nixos-rebuild build
```

This command will build the new system configuration, but won't activate it nor
add into the GRUB boot menu. When building NixOS machines with a continuous
integration server you'll use `nixos-rebuild build` is the primary verb you'll
use.

If the build was successful then it's time to save your progress:

```bash
cd /etc/nixos
git add -A
git commit -m "checkpoint commit"
```

If the compilation failed, resolve them by correcting
`/etc/nixos/configuration.nix` before proceeding.

## 🎯 Test the operating system configuration

Now that your operating system configuration is successfully building, let's try
out the changes in a non-commital way:

```bash
nixos-rebuild test
```

This command builds and activates the new configuration, but does not add it to
the GRUB boot menu. Thus, if you reboot the system (or if it crashes), you will
automatically revert to the default configuration (i.e. the configuration
resulting from the last call to nixos-rebuild switch or nixos-rebuild boot).

Now imagine, your ssh-ed into a machine and accidentally made a mistake to the
configuration which locked out your network access. Because you used
`nixos-rebuild test` to activate and validate your changes _before_ committing
them via `nixos-rebuild switch` you can roll back mistakes by simply rebooting
the computer.

Let's validate your changes have been applied and activated but not committed:

```bash
# htop, a command which wasn't previously installed now works
htop
```

Before continuing, remember that running `nixos-rebuild test` meant that the
changes to your `configuration.nix` got temporarily applied, including the new
user you created.

So now you can create a password for your user. Remember to substitute your
username if you used a different one from `workshop`:

```bash
passwd workshop
```

Reboot the computer and log back in as `root`. When you re-run the `htop`
command you'll notice that it isn't installed anymore.

```bash
# htop: command not found
htop
```

Your user is not there anymore, though the `/home/$username` directory will
still be there. NixOS does the sane thing and doesn't delete your data, even if
your user is wiped from /etc/passwd

```bash
# No passwd entry for user 'workshop'
su -l workshop
ls /home
```

> 🛈 If you need to validate changes in continuous integration scenarios or
> during initial development, instead of using `nixos-rebuild test` consider
> using the `nixos-rebuild build-vm` command instead. This command builds a
> virtual machine with the configuration defined in
> `/etc/nixos/configuration.nix`. Once the virtual machine has been built the
> virtual machine can be started by running `./result/bin/run-*-vm`.


## 📚 Structure of a configuration.nix file

A `configuration.nix` file is really a function definition in the Nix language.
It usually starts with `{config, pkgs, ... }:` which means it takes two
arguments: `config` and `pkgs`. Then it returns a set of option definitions
(key/value pairs of the form `{ option = definition; }`) that, processed by
nixos-rebuild, updates the system. We'll explain later how this works.

We've included a sample file in the repo that we recommend you look
at as you go through this description.

* `boot`: Most systems will use Grub 2, which can be configured to boot with BIOS or UEFI.
* `fileSystems`: Defines and mounts filesystems, but changing this
    configuration will only affect mount options, not formatting. That is: if
    you have a `vfat` filesystem and change the `fsType` option to `ext4` and
    rebuild your system, the mount will stop working, but your data won't be
    compromised by a reformatting.
* `imports`: Nix files are modular, and `configuration.nix` is no exception.
    The default `configuration.nix` generated by a clean install with
    `nixos-install` will generate a second `hardware-configuration.nix` that
    gets imported at the head of the file. Another example: the server running
    the nixpkgs cache-proxy for this workshop is configured by including a .nix
    file into its `configuration.nix`:

```nix
imports =
    [ # Include the results of the hardware scan.
    ./hardware-configuration.nix
    # Binary cache proxy for the NixOS tutorial at Compose 2019
    ./nix-binary-cache-proxy.nix
    ];
```

* `nix`: Configuration for the Nix package manager itself. you can add new
    binary caches for built packages, or public keys for the new binary caches.
    For instance, if you want to save yourself compiling the `leksah` editor for
    Haskell, you can use the public binaries compiled by IOHK:

```nix
nix.binaryCaches = [ 
    "https://hydra.iohk.io"
    "http://cache.nixos.org/"
];

nix.binaryCachePublicKeys = [
    "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ= cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
];
```

* `hardware`: What it says on the tin. Mostly binary blobs and proprietary
    drivers.
* `nixpkgs`: Configuration about which package sets you want discoverable on
    your system. The following configuration options speak for themselves, so
    the comments explain why one might want to use them:

```nix
nixpkgs.config = {
    # Mostly because of the need for MicroSoft fonts and Steam:
    allowUnfree = true;
    # because EVDI/displaylink was broken on the update to 19.03 Koi
    # but not broken enough that it was completely useless:
    allowBroken = true;
};
```

The `nixpkgs.overlay` attribute can also be used to extend and change the
set of available packages. It's NixOS's equivalent to Debian package
pinning.

* `time`: Configure your system's timezone.

```nix
time.timeZone = "Australia/Melbourne";
```

* `i18n`: Configure your system's font, keymap, locale...

```nix
i18n = {
    consoleFont = "Fira Mono";
    consoleKeyMap = "us";
    defaultLocale = "en_AU.UTF-8";
};
```

* `networking`: Set your system's hostname, enable a firewall and define open
    ports.
* `environment`: This defines the default profile's environment. Among other
    things, this means that here we can configure the applications that will be
    available to the root profile and to users' profiles by default.
* `services`: NixOS has a number of systemd-configured services it can run.
    From openssh to nginx, from avahi/mdns to printing... even the X11 windowing
    system is defined as a NixOS/systemd service.
* `Virtualisation`: virtualbox, xen, libvirt and docker are among the
    pre-defined virtualsation services that NixOS offers. Note that these aren't
    installed as applications! Let's enable all of them at the same time:

```nix
virtualisation.virtualbox.host.enable = true;
nixpkgs.config.virtualbox.host.enableExtensionPack = true;

virtualisation.libvirtd.enable = true;

virtualisation.docker.enable = true;
```

Note that, for users to be able to use virtualisation, they need to be added
to the corresponding unix groups (see below).

* `users`: User and group creation is also managed as part of NixOS: removing
    a user and running `nixos-rebuild` will not delete their `/home/username`
    directory, but will delete them from the generated `/etc` files in the
    current profile as if they'd never existed.

```nix
users.extraUsers.nedkelly = {
    isNormalUser = true;
    description = "Ned Kelly";
    extraGroups = [
    "adbusers"
    "audio" "disk" "docker" "networkmanager" "plugdev"
    "systemd-journal" "wheel" "vboxusers" "libvirtd" "video"
    ];
    uid = 1000;
};

users.defaultUserShell = "/run/current-system/sw/bin/bash";
```

Please remember to give your new user a password with `passwd`.

## ⏭️ What's next

An advantage of having a purely functional operating system is that system
configurations are immutable. You can think of them (metaphorically) as git
commits that can be checked out as easily as the text of the `configuration.nix`
file itself. Next we'll learn [how to roll back a system configuration].

<!-- in-line links -->
[fhs-standard]: https://en.wikipedia.org/wiki/Filesystem_Hierarchy_Standard
[how to roll back a system configuration]: ../04-roll-back-configuration/README.md
