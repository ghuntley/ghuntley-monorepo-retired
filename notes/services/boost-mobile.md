---
title: boost mobile
layout: notes
---

# data usage

``` shell
$ 0 11 * * * /usr/bin/wget http://care.boost.com.au -O /home/pi/datausage/care`date +\%Y\%m\%d`.txt
$ grep Expires care*.txt
```
