ACPI Table Extraction / SeaBIOS SLIC Integration
================================================

If your computer is installed with Windows 7/8 by default but you'd prefer to run Linux as your desktop but on ocassion run a single Windows Virtual Machine under KVM using the activiation credentials within your computers BIOS then SeaSLIC can help you achieve this.


### Debian

As of 6th of Apr 2014 thanks to the excellent work by Michael Tokarev this patch is now integrated by default into Debian which removes the need to roll and maintain your own your own copy of SeaBIOS.

    # sudo xxd -i /sys/firmware/acpi/tables/SLIC | grep -v -E "len "| sed 's/unsigned char.*/static char SLIC[] = {/' > /tmp/slic.bin

    # qemu-system-x86_64 -acpitable file=[/tmp/slic.bin | /sys/firmware/acpi/tables/SLIC]
    

### Other

Download the latest SeaBIOS version and put the archive content into the 'seabios.module' directory.

    ./patch.sh
    cp /usr/share/qemu/bios.bin /usr/share/qemu/bios-orig.bin
    cp seabios.submodule/out/bios.bin /usr/share/qemu/bios.bin

Please ensure that the packages 'iasl' and 'vim-common' are installed before compiling.
