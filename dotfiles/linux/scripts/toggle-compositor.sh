#!/bin/sh
# Copyright (c) 2020 Geoffrey Huntley <ghuntley@ghuntley.com>. All rights reserved.
# SPDX-License-Identifier: Proprietary

pid=$(pidof compton)
if [ $pid ]; then
  echo "Disabling compositor"
  killall compton
else
  echo "Enabling compositor"
  compton -cCFb --backend glx --vsync opengl
fi

