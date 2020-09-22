#!/usr/bin/env bash
# Copyright (c) 2020 Geoffrey Huntley <ghuntley@ghuntley.com>. All rights reserved.
# SPDX-License-Identifier: Proprietary


killall -q polybar
while pgrep -u $UID -x polybar >/dev/null; do sleep 1; done
polybar ghuntley &

if type "xrandr"; then
  for m in $(xrandr --query | grep " connected" | cut -d" " -f1); do
    MONITOR=$m polybar --reload ghuntley &
  done
else
  polybar --reload ghuntley &
fi

