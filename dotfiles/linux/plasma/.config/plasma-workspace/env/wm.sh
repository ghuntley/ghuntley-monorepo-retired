# Disable KWin and use i3gaps as WM
export KDEWM=/run/current-system/sw/bin/i3


# Compositor (Animations, Shadows, Transparency)
# xcompmgr -C
compton -b --config ~/.config/compton/compton.conf
