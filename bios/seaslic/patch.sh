#!/bin/bash
# Copyright (c) 2020 Geoffrey Huntley <ghuntley@ghuntley.com>. All rights reserved.
# SPDX-License-Identifier: Proprietary

#set -xv
export HEADER="==========================================================================="

echo
echo $HEADER
echo "dump the slic from motherboard (root password required)"
echo $HEADER
echo

# dump the slic table from the computer (note: requires root)
sudo xxd -i /sys/firmware/acpi/tables/SLIC | grep -v -E "len "| sed 's/unsigned char.*/static char SLIC[] = {/' > seabios.submodule/src/acpi-slic.hex

echo
echo $HEADER
echo patching seabios...
echo $HEADER
echo

cd seabios.submodule
patch -p1 < ../seabios.patch

echo
echo $HEADER
echo compiling seabios...
echo $HEADER
echo

make

echo
echo $HEADER
echo your bios awaits....
echo $HEADER
echo

cd ..
ls seabios.submodule/out/bios.bin

