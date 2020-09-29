# entropy

Assert entropy_avail > 1024, if below this amount then haveged may have malfunctioned:

```
cat /proc/sys/kernel/random/entropy_avail
```



