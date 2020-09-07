# Copyright (c) 2020 Geoffrey Huntley <ghuntley@ghuntley.com>. All rights reserved.
# SPDX-License-Identifier: Proprietary

# Disable KWin and use i3gaps as WM
export KDEWM=/run/current-system/sw/bin/i3


# Compositor (Animations, Shadows, Transparency)
# xcompmgr -C
compton -b --config ~/.config/compton/compton.conf

