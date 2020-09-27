#!/usr/bin/env bash

keychain --clear --agents gpg

bat=$(acpi -a)
if [ "$bat" = "Adapter 0: on-line" ]; then
  sudo systemctl start physlock
  xset dpms force off
else
  systemctl suspend
fi

