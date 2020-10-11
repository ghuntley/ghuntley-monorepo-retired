#!/usr/bin/env bash
# Copyright (c) 2020 Geoffrey Huntley <ghuntley@ghuntley.com>. All rights reserved.
# SPDX-License-Identifier: Proprietary


keychain --clear --agents gpg

bat=$(acpi -a)
if [ "$bat" = "Adapter 0: on-line" ]; then
  sudo systemctl start physlock
  xset dpms force off
else
  systemctl suspend
fi


