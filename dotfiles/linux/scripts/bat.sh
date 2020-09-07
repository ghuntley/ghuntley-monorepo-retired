#!/usr/bin/env bash
# Copyright (c) 2020 Geoffrey Huntley <ghuntley@ghuntley.com>. All rights reserved.
# SPDX-License-Identifier: Proprietary

# Battery status (systray)
AC=$(</sys/class/power_supply/AC/online)

# Battery is online: ICON % [CHARGING ICON]
if [ -f /sys/class/power_supply/BAT0/capacity ]; then
  #killall compton
  BAT=$(</sys/class/power_supply/BAT0/capacity)
  if [ $BAT -lt 11 ]; then
    echo "ï"
  elif [ $BAT -gt 10 ] && [ $BAT -lt 31 ]; then
    echo "ï"
  elif [ $BAT -gt 30 ] && [ $BAT -lt 61 ]; then
    echo "ï"
  elif [ $BAT -gt 60 ] && [ $BAT -lt 81 ]; then
    echo "ï"
  elif [ $BAT -gt 80 ]; then
    echo "ï"
  fi
  echo "$BAT%"
  # Online and charging
  if [ $AC -eq 1 ]; then
    echo "ï§"
  fi
else
  # AC only
  echo "ï¦"
fi

# Compositor Icon
pid=$(pidof compton)
if [ $pid ]; then
  echo "&nbsp;ï"
fi

