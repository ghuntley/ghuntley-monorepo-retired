---
title: apple usb network adapter
layout: notes
---

On the back of the packaging of the Apple USB Network Adapter there's a little disclaimer that mentions that it can be _only_ used with a Apple MacBook. This is not true, I have been for a couple years using the adapter on Windows Vista, 7, 8.x and 10 with [this driver](/downloads/apple-usb-networking-driver-for-windows-vista-7-8-10.zip). The driver was created by myself by taking the official OEM vendor driver and adding the appropriate Apple identifier into ```Ax88722.inf``` as per [these instructions](http://ashleyangell.com/2010/09/windows-7-drivers-for-apples-usb-ethernet-adapter/).

![Apple USB Network Adapter](/images/apple-usb-network-adapter.png)

Here's how you can install it:

* [Download the network driver](/downloads/apple-usb-networking-driver-for-windows-vista-7-8-10.zip) and extract it.
* Next, attach your Apple USB Ethernet Adapter if you have not already done so.
* Launch Device Manager (right-click on "computer" and select "manage").
* Locate the unknown device "Apple USB Ethernet".
* Right click on the device and select "Update Driver Software".
* Select "browse my computer for driver software"
* In the file browser dialog, select the driver thats appropriate to your operating system (32bit vs 64bit)
* Done!
