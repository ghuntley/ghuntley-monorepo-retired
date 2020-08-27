---
title: windows phone
---

A quick tech-tip on how to resolve the issue whereby the Windows Phone 8.x or 10.x emulators stall upon launch at the initial "Windows Phone OS is starting..." loading screen.

The root cause is usually related to invalid or corrupted Hyper-V networking configuration and rather than waste time troubleshooting, I recommend just resetting the emulators back to factory defaults:

    Close all emulators and visual studio instances that may be running.
    Assuming you do not have custom virtual machines defined, in Hyper-V Manager delete all virtual machines and all virtual switches

Finally run xdecleanup.exe as administrator for every version of XDE installed

# C:\progra~2\Microsoft XDE\8.0\XdeCleanup.exe
# C:\progra~2\Microsoft XDE\8.1\XdeCleanup.exe
# C:\progra~2\Microsoft XDE\10.0.10586.0\XdeCleanup.exe
