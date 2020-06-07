---
title: vmware esxi
---

# list all logical drives

```
[root@esxi:~] esxcfg-scsidevs -c
Device UID                   Device Type      Console Device                                     Size      Multipath PluginDisplay Name
mpx.vmhba32:C0:T0:L0         Direct-Access    /vmfs/devices/disks/mpx.vmhba32:C0:T0:L0           7651MB    NMP     Local USB Direct-Access (mpx.vmhba32:C0:T0:L0)
naa.5000c500444f097e         Direct-Access    /vmfs/devices/disks/naa.5000c500444f097e           305245MB  NMP     Local ATA Disk (naa.5000c500444f097e)
naa.50014ee20fc725d0         Direct-Access    /vmfs/devices/disks/naa.50014ee20fc725d0           5723166MB NMP     Local ATA Disk (naa.50014ee20fc725d0)
naa.50014ee262abd843         Direct-Access    /vmfs/devices/disks/naa.50014ee262abd843           2861588MB NMP     Local ATA Disk (naa.50014ee262abd843)
naa.50014ee262abe0ae         Direct-Access    /vmfs/devices/disks/naa.50014ee262abe0ae           2861588MB NMP     Local ATA Disk (naa.50014ee262abe0ae)
naa.50014ee262abec25         Direct-Access    /vmfs/devices/disks/naa.50014ee262abec25           2861588MB NMP     Local ATA Disk (naa.50014ee262abec25)
naa.50014ee2b801aa02         Direct-Access    /vmfs/devices/disks/naa.50014ee2b801aa02           2861588MB NMP     Local ATA Disk (naa.50014ee2b801aa02)
naa.50014ee608915404         Direct-Access    /vmfs/devices/disks/naa.50014ee608915404           2861588MB NMP     Local ATA Disk (naa.50014ee608915404)
naa.50014ee65de68b90         Direct-Access    /vmfs/devices/disks/naa.50014ee65de68b90           2861588MB NMP     Local ATA Disk (naa.50014ee65de68b90)
naa.50014ee6b33beccc         Direct-Access    /vmfs/devices/disks/naa.50014ee6b33beccc           2861588MB NMP     Local ATA Disk (naa.50014ee6b33beccc)
naa.50014ee6b33cc37f         Direct-Access    /vmfs/devices/disks/naa.50014ee6b33cc37f           2861588MB NMP     Local ATA Disk (naa.50014ee6b33cc37f)
naa.50014ee6b33cc945         Direct-Access    /vmfs/devices/disks/naa.50014ee6b33cc945           2861588MB NMP     Local ATA Disk (naa.50014ee6b33cc945)
naa.50014ee6b33cd08a         Direct-Access    /vmfs/devices/disks/naa.50014ee6b33cd08a           2861588MB NMP     Local ATA Disk (naa.50014ee6b33cd08a)
naa.50025388a059c7bd         Direct-Access    /vmfs/devices/disks/naa.50025388a059c7bd           953869MB  NMP     Local ATA Disk (naa.50025388a059c7bd)
t10.DP______BACKPLANE000000  Enclosure Svc Dev/vmfs/devices/genscsi/t10.DP______BACKPLANE000000  0MB       NMP     Local DP Enclosure Svc Dev (t10.DP______BACKPLANE000000)
```


# sata pass through

```
esxcfg-scsidevs -c |grep 286 | awk '{print "vmkfstools -z " $3 " nas-disk-" $1 ".vmdk"}'
 
 
export DRIVE=/vmfs/devices/disks/naa.50014ee2b687de02
export SERIAL=unknown

vmkfstools -z $DRIVE /vmfs/volumes/5b86eeca-c8414b34-e1db-74867ade692a/nas/nas-disk-$SERIAL.vmdk
```

# remount nfs storage

```
esxcli storage nfs remove -v nas
esxcli storage nfs add -H nas.infrastructure -s /mnt/tank/vmfs -v nas
```
