---
title: systemd
layout: notes
---

# Logging

If you know the exact PID of the process you are interested in, you can filter by that as well by specifying the `_PID` field.
 
```shell
journalctl _PID=8088
```

# References
* https://www.digitalocean.com/community/tutorials/how-to-use-journalctl-to-view-and-manipulate-systemd-logs
