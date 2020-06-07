# Prerequisites

## 📖 Overview

If you are doing this workshop as part of a classroom, it is important to
pre-install these components before attendance as you'll need to download
upwards of 4Gb of software from the internet.

## ☑️ Software Installation

Attendees will need to bring their own laptop, and download and install the
following software:

* [ ] Download the [NixOS graphical installation ISO][download-nixos-iso] which contains the NixOS installer.
* [ ] Download and install [VirtualBox][download-virtualbox].
* [ ] Download and install [VirtualBox Extension Pack][download-virtualbox-extension-pack].

## ☑️ Configuration

If you are installing NixOS on a laptop as its primary operating system then
you'll need to [burn the installation ISO to a USB drive or DVD][burn-the-iso].

Alternatively, if you are going to use VirtualBox then you'll need to:

* [ ] In VirtualBox, use the menu option `Machine -> New` and configure as follows to create a virtual machine for NixOS:

    * [ ] **NixOS**: `NixOS`
    * [ ] **Type**: `Linux`
    * [ ] **Version**: `Linux 2.6 / 3.x / 4.x (64-bit)`
    * [ ] **Memory**: Raise memory to at least `6GB`.
    * [ ] **Hard Drive**: Allocate now and set size to at least `20GB`.

* [ ] In VirtualBox, use the menu option `Machine -> Settings` and configure the virtual machine as follows:

    * [ ] **Display**: Ensure that the graphics Controller is set as `VBoxVGA` (not SVGA) and Video Memory is set to least `64Mb`.
    * [ ] **Storage**: Under `Controller IDE`, attach the [NixOS graphical installation ISO][download-nixos-iso].

Windows users, if Hyper-V is enabled on your computer [you'll need to follow
these steps][bcd-edit] from Scott Hanselman before VirtualBox will work on your
computer.

## What's next

🎉 that's it. You're now ready to learn all about the NixOS operating system. In
the [next module][next-module] you'll learn all about NixOS and install the
operating system.

<!-- in-line links -->
[burn-the-iso]: https://nixos.org/nixos/manual/index.html#sec-booting-from-usb
[bcd-edit]: https://www.hanselman.com/blog/SwitchEasilyBetweenVirtualBoxAndHyperVWithABCDEditBootEntryInWindows81.aspx

[download-virtualbox]: https://www.virtualbox.org/wiki/Downloads
[download-virtualbox-extension-pack]: https://download.virtualbox.org/virtualbox/6.0.10/Oracle_VM_VirtualBox_Extension_Pack-6.0.10.vbox-extpack
[download-nixos-iso]: https://releases.nixos.org/nixos/19.03/nixos-19.03.173307.776d66ec115/nixos-graphical-19.03.173307.776d66ec115-x86_64-linux.iso
[download-nixos-ova]: https://releases.nixos.org/nixos/19.03/nixos-19.03.173201.defa89ffaef/nixos-19.03.173201.defa89ffaef-x86_64-linux.ova


[next-module]: ../01-introduction-to-nixos/README.md
