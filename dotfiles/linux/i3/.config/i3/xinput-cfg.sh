#!/usr/bin/env bash
# Copyright (c) 2020 Geoffrey Huntley <ghuntley@ghuntley.com>. All rights reserved.
# SPDX-License-Identifier: Proprietary


# Kensington Slimblade
xinput set-int-prop "Kensington Kensington Slimblade Trackball" "Evdev Middle Button Emulation" 8 0 7 8 9
xinput set-button-map "Kensington Kensington Slimblade Trackball" 1 8 2 4 5 6 7 3 2
xinput set-int-prop "Kensington Kensington Slimblade Trackball" "Evdev Wheel Emulation" 8 1
xinput set-int-prop "Kensington Kensington Slimblade Trackball" "Evdev Wheel Emulation Button" 8 8
xinput set-int-prop "Kensington Kensington Slimblade Trackball" "Evdev Wheel Emulation Axes" 8 6 7 4 5
xinput set-int-prop "Kensington Kensington Slimblade Trackball" "Evdev Wheel Emulation Timeout" 16 300
xinput set-prop "Kensington Kensington Slimblade Trackball" "libinput Accel Speed" 1


